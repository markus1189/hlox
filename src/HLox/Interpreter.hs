module HLox.Interpreter (interpret, eval, evalPure) where

import Control.Applicative ((<|>))
import Control.Lens (at)
import Control.Lens.Combinators (to, use, view)
import Control.Lens.Operators
  ( (%=),
    (&),
    (?=),
    (?~),
    (^.),
    (^?), (<<.=), (.=),
  )
import Control.Monad (unless, void)
import Control.Monad.Error.Class (liftEither, tryError)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (whileM_)
import Control.Monad.RWS (MonadState, MonadWriter)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (for_, traverse_)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import HLox.Interpreter.Types
import HLox.Parser.Types (Expr (..), Stmt (..))
import HLox.Scanner.Types
import HLox.Types (Lox)
import HLox.Util (loxRuntimeError)

eval :: (Traversable t) => t Stmt -> Lox ()
eval stmts = do
  r <- runExceptT $ runWriterT $ flip evalStateT initialEnv $ evalPure stmts
  case r of
    Left err -> loxRuntimeError err
    Right (_, effs) -> do liftIO $ for_ effs runEffect

evalPure :: (Traversable t, MonadIO m, MonadState Environment m, MonadError InterpretError m, MonadWriter [LoxEffect] m) => t Stmt -> m ()
evalPure = traverse_ executePure
  where
    executePure (StmtIf c t f) = do
      c' <- interpret c
      if isTruthy c'
        then executePure t
        else traverse_ executePure f
    executePure (StmtExpr e) = void $ interpret e
    executePure (StmtPrint e) = do
      x <- interpret e
      tell [LoxEffectPrint $ stringify x]
    executePure (StmtVar name Nothing) = do
      at (name ^. lexeme . _Lexeme) ?= LoxNil
      pure ()
    executePure (StmtVar name (Just initializer)) = do
      v <- interpret initializer
      let name' = name ^. lexeme . _Lexeme
      at name' ?= v
      pure ()
    executePure (StmtBlock stmts') = do
      id %= pushEmptyEnv
      traverse_ executePure stmts'
      id %= popEnv
      pure ()
    executePure (StmtWhile cond body) = whileM_ (isTruthy <$> interpret cond) (executePure body)
    executePure (StmtFunction name params body) = do
      env <- use id
      let f = LoxFun (map (view (lexeme . _Lexeme)) params) env' body
          env' = env & at name' ?~ f
          name' = view (lexeme . _Lexeme) name
      id .= env'
    executePure (StmtReturn _ value) = do
      r <- case value of
        Just v -> interpret v
        Nothing -> pure LoxNil
      throwError (InterpretReturn r)

runEffect :: LoxEffect -> IO ()
runEffect (LoxEffectPrint t) = TIO.putStrLn t

interpret :: (MonadIO m, MonadState Environment m, MonadError InterpretError m, MonadWriter [LoxEffect] m) => Expr -> m LoxValue
--
interpret (ExprLiteral LitNothing) = pure LoxNil
interpret (ExprLiteral (LitText v)) = pure $ LoxText v
interpret (ExprLiteral (LitNumber v)) = pure $ LoxNumber v
interpret (ExprLiteral (LitBool v)) = pure $ LoxBool v
--
interpret (ExprGrouping e) = interpret e
--
interpret (ExprUnary t e) = case t ^. tokenType of
  BANG -> LoxBool . not . isTruthy <$> interpret e
  MINUS -> do
    right <- interpret e
    case right of
      LoxNumber n -> pure $ LoxNumber (negate n)
      _ -> throwError $ InterpretRuntimeError t "Unary minus on invalid value"
  _ -> throwError $ InterpretRuntimeError t "Unmatched case"
--
interpret (ExprBinary lhs op rhs) = do
  e1 <- interpret lhs
  e2 <- interpret rhs
  case opType of
    BANG_EQUAL -> pure $ LoxBool $ e1 /= e2
    EQUAL_EQUAL -> pure $ LoxBool $ e1 == e2
    --
    GREATER -> applyBinaryBool (>) e1 e2 "Invalid > operands"
    GREATER_EQUAL -> applyBinaryBool (>=) e1 e2 "Invalid >= operands"
    LESS -> applyBinaryBool (<) e1 e2 "Invalid < operands"
    LESS_EQUAL -> applyBinaryBool (<=) e1 e2 "Invalid <= operands"
    --
    MINUS -> applyBinaryNumber (-) e1 e2 "Invalid minus operands"
    SLASH -> applyBinaryNumber (/) e1 e2 "Invalid div operands"
    STAR -> applyBinaryNumber (*) e1 e2 "Invalid mult operands"
    PLUS ->
      liftEither $
        maybeToRight (InterpretRuntimeError op "Invalid plus operands") $
          let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber
              addStrings = fmap LoxText $ Text.append <$> e1 ^? to stringify <*> e2 ^? to stringify
           in addNumbers <|> addStrings
    _ -> throwError (InterpretRuntimeError op "Invalid operand case")
  where
    opType = op ^. tokenType
    applyBinaryNumber f e1 e2 msg = liftEither $ maybeToRight (InterpretRuntimeError op msg) $ fmap LoxNumber (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    applyBinaryBool f e1 e2 msg = liftEither $ maybeToRight (InterpretRuntimeError op msg) $ fmap LoxBool (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
--
interpret (ExprAssign name value) = do
  let name' = name ^. lexeme . _Lexeme
  v <- interpret value
  r <- use (at name')
  case r of
    Nothing -> throwError (InterpretRuntimeError name [i|Undefined variable '#{name'}'|])
    Just _ -> at name' ?= v
  pure v
--
interpret (ExprVariable name) = do
  let name' = name ^. lexeme . _Lexeme
  mv <- use $ at name'
  case mv of
    Just v -> pure v
    Nothing -> throwError $ InterpretRuntimeError name [i|Undefined variable '#{name'}'|]
--
interpret (ExprLogical lhs op rhs) = do
  left <- interpret lhs
  if op ^. tokenType == OR && isTruthy left || op ^. tokenType == AND && not (isTruthy left)
    then pure left
    else interpret rhs
--
interpret (ExprCall callee paren arguments) = do
  callee' <- interpret callee
  args <- traverse interpret arguments
  checkArity paren (length args) callee'
  let function = callee'
  call paren function args

call :: (MonadIO m, MonadError InterpretError m, MonadState Environment m, MonadWriter [LoxEffect] m) => Token -> LoxValue -> [LoxValue] -> m LoxValue
call _ (LoxFun params funEnv body) args = do
  let funEnvWithParams = foldl' (\acc (k, v) -> acc & at k ?~ v) funEnv (params `zip` args)
  oldEnv <- id <<.= funEnvWithParams
  r <- tryError $ evalPure body
  r' <- case r of
    Left (InterpretReturn v) -> pure $ Just v
    Left e@(InterpretRuntimeError _ _) -> throwError e
    Right _ -> pure Nothing
  id .= oldEnv
  pure (fromMaybe LoxNil r')
call _ (LoxNativeFun LoxClock) _ = LoxNumber . realToFrac <$> liftIO getPOSIXTime
call paren _ _ = throwError (InterpretRuntimeError paren "Can only call functions and classes.")

checkArity :: (MonadError InterpretError m) => Token -> Int -> LoxValue -> m ()
checkArity paren a1 (LoxFun a2 _ _) = unless (a1 == length a2) $ throwError (InterpretRuntimeError paren [i|Expected #{length a2} arguments but got #{a1}.|])
checkArity paren a1 (LoxNativeFun LoxClock) = unless (a1 == 0) $ throwError (InterpretRuntimeError paren [i|Expected 0 arguments but got #{a1}.|])
checkArity _ _ _ = pure ()

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

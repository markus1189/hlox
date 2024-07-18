module HLox.Interpreter (interpret, eval, evalPure) where

import Control.Applicative ((<|>))
import Control.Lens.Combinators (to, use)
import Control.Lens.Operators ((%=), (^.), (^?))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (MonadState)
import Control.Monad.State (StateT, evalStateT)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Interpreter.Types
import HLox.Parser.Types (Expr (..), Stmt (..))
import HLox.Scanner.Types
import HLox.Types (Lox)
import HLox.Util (loxRuntimeError)

eval :: (Traversable t) => t Stmt -> Lox ()
eval stmts = do
  let r = evalPure stmts
  case r of
    Left err -> loxRuntimeError err
    Right stmtValues -> liftIO $ for_ stmtValues execute

evalPure :: (Traversable t) => t Stmt -> Either InterpretError (t LoxStmtValue)
evalPure stmts = evalStateT (traverse executePure stmts) envEmpty
  where
    executePure :: Stmt -> StateT Environment (Either InterpretError) LoxStmtValue
    executePure (StmtExpr e) = LoxStmtVoid <$ interpret e
    executePure (StmtPrint e) = do
      x <- interpret e
      pure $ LoxStmtPrint $ stringify x
    executePure (StmtVar name Nothing) = do
      id %= envDefine (name ^. lexeme . _Lexeme) LoxNil
      pure LoxStmtVoid
    executePure (StmtVar name (Just initializer)) = do
      v <- interpret initializer
      let name' = name ^. lexeme . _Lexeme
      id %= envDefine name' v
      pure LoxStmtVoid

execute :: LoxStmtValue -> IO ()
execute LoxStmtVoid = pure ()
execute (LoxStmtPrint t) = TIO.putStrLn t

interpret :: (MonadState Environment m, MonadError InterpretError m) => Expr -> m LoxValue
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
      _ -> throwError $ InterpretError t "Unary minus on invalid value"
  _ -> throwError $ InterpretError t "Unmatched case"
--
interpret (ExprBinary lhs op rhs) = do
  e1 <- interpret lhs
  e2 <- interpret rhs
  case opType of
    GREATER ->
      liftEither $ maybeToRight (InterpretError op "Invalid operands") $ fmap LoxBool ((>) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    GREATER_EQUAL -> liftEither $ maybeToRight (InterpretError op "Invalid >= operands") $ fmap LoxBool ((>=) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    LESS -> liftEither $ maybeToRight (InterpretError op "Invalid < operands") $ fmap LoxBool ((<) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    LESS_EQUAL -> liftEither $ maybeToRight (InterpretError op "Invalid <= operands") $ fmap LoxBool ((<=) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    BANG_EQUAL -> pure $ LoxBool $ e1 /= e2
    EQUAL_EQUAL -> pure $ LoxBool $ e1 == e2
    MINUS ->
      liftEither $ maybeToRight (InterpretError op "Invalid minus operands") $ fmap LoxNumber ((-) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    SLASH ->
      liftEither $ maybeToRight (InterpretError op "Invalid div operands") $ fmap LoxNumber ((/) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    STAR ->
      liftEither $ maybeToRight (InterpretError op "Invalid mult operands") $ fmap LoxNumber ((*) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    PLUS ->
      liftEither $
        maybeToRight (InterpretError op "Invalid plus operands") $
          let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber
              addStrings = fmap LoxText $ Text.append <$> e1 ^? to stringify <*> e2 ^? to stringify
           in addNumbers <|> addStrings
    _ -> throwError (InterpretError op "Invalid operand case")
  where
    opType = op ^. tokenType
interpret (ExprAssign name value) = do
  let name' = name ^. lexeme . _Lexeme
  v <- interpret value
  env <- use id
  let r = envGet name' env
  case r of
    Nothing -> throwError (InterpretError name [i|Undefined variable '#{name'}'|])
    Just _ -> id %= envDefine name' v
  pure v
--
interpret (ExprVariable name) = do
  let name' = name ^. lexeme . _Lexeme
  mv <- use $ to (envGet name')
  case mv of
    Just v -> pure v
    Nothing -> throwError $ InterpretError name [i|Undefined variable '#{name'}'|]

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

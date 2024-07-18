module HLox.Interpreter (interpret, eval, evalPure) where

import Control.Applicative ((<|>))
import Control.Lens (at)
import Control.Lens.Combinators (to, use)
import Control.Lens.Operators ((.=), (?=), (^.), (^?))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (MonadState)
import Control.Monad.State (StateT, evalStateT)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (for_, traverse_)
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
evalPure stmts = evalStateT (traverse executePure stmts) mempty
  where
    executePure :: Stmt -> StateT Environment (Either InterpretError) LoxStmtValue
    executePure (StmtExpr e) = LoxStmtVoid <$ interpret e
    executePure (StmtPrint e) = do
      x <- interpret e
      pure $ LoxStmtPrint $ stringify x
    executePure (StmtVar name Nothing) = do
      at (name ^. lexeme . _Lexeme) ?= LoxNil
      pure LoxStmtVoid
    executePure (StmtVar name (Just initializer)) = do
      v <- interpret initializer
      let name' = name ^. lexeme . _Lexeme
      at name' ?= v
      pure LoxStmtVoid
    executePure (StmtBlock stmts') = do
      env <- use id
      xs <- traverse executePure stmts'
      id .= env
      pure (LoxStmtBlock xs)

execute :: LoxStmtValue -> IO ()
execute LoxStmtVoid = pure ()
execute (LoxStmtPrint t) = TIO.putStrLn t
execute (LoxStmtBlock vs) = traverse_ execute vs

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
        maybeToRight (InterpretError op "Invalid plus operands") $
          let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber
              addStrings = fmap LoxText $ Text.append <$> e1 ^? to stringify <*> e2 ^? to stringify
           in addNumbers <|> addStrings
    _ -> throwError (InterpretError op "Invalid operand case")
  where
    opType = op ^. tokenType
    applyBinaryNumber f e1 e2 msg = liftEither $ maybeToRight (InterpretError op msg) $ fmap LoxNumber (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    applyBinaryBool f e1 e2 msg = liftEither $ maybeToRight (InterpretError op msg) $ fmap LoxBool (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
interpret (ExprAssign name value) = do
  let name' = name ^. lexeme . _Lexeme
  v <- interpret value
  r <- use (at name')
  case r of
    Nothing -> throwError (InterpretError name [i|Undefined variable '#{name'}'|])
    Just _ -> at name' ?= v
  pure v
--
interpret (ExprVariable name) = do
  let name' = name ^. lexeme . _Lexeme
  mv <- use $ at name'
  case mv of
    Just v -> pure v
    Nothing -> throwError $ InterpretError name [i|Undefined variable '#{name'}'|]

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

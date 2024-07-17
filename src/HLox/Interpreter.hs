module HLox.Interpreter (interpret, eval, evalPure) where

import Control.Applicative ((<|>))
import Control.Lens.Combinators (to, use, view, _Right)
import Control.Lens.Operators ((%=), (^.), (^?))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (runReader)
import Control.Monad.State (StateT, evalStateT, lift)
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
    executePure (StmtExpr e) = do
      env <- use id
      lift $ LoxStmtVoid <$ runReader (interpret e) env
    executePure (StmtPrint e) = do
      env <- use id
      lift $ LoxStmtPrint . stringify <$> runReader (interpret e) env
    executePure (StmtVar name Nothing) = do
      id %= envDefine (name ^. lexeme . _Lexeme) LoxNil
      pure LoxStmtVoid
    executePure (StmtVar name (Just initializer)) = do
      env <- use id
      v <- lift $ runReader (interpret initializer) env
      let name' = name ^. lexeme . _Lexeme
      id %= envDefine name' v
      pure LoxStmtVoid

execute :: LoxStmtValue -> IO ()
execute LoxStmtVoid = pure ()
execute (LoxStmtPrint t) = TIO.putStrLn t

interpret :: (MonadReader Environment m) => Expr -> m (Either InterpretError LoxValue)
--
interpret (ExprLiteral LitNothing) = pure $ Right LoxNil
interpret (ExprLiteral (LitText v)) = pure $ Right $ LoxText v
interpret (ExprLiteral (LitNumber v)) = pure $ Right $ LoxNumber v
interpret (ExprLiteral (LitBool v)) = pure $ Right $ LoxBool v
--
interpret (ExprGrouping e) = interpret e
--
interpret (ExprUnary t e) = case t ^. tokenType of
  BANG -> fmap (LoxBool . not . isTruthy) <$> interpret e
  MINUS -> runExceptT $ do
    right <- ExceptT (interpret e)
    case right of
      LoxNumber n -> pure $ LoxNumber (negate n)
      _ -> throwError $ InterpretError t "Unary minus on invalid value"
  _ -> pure $ Left $ InterpretError t "Unmatched case"
--
interpret (ExprBinary lhs op rhs) = do
  e1 <- interpret lhs
  e2 <- interpret rhs
  case opType of
    GREATER -> pure $ maybeToRight (InterpretError op "Invalid operands") $ fmap LoxBool ((>) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    GREATER_EQUAL -> pure $ maybeToRight (InterpretError op "Invalid >= operands") $ fmap LoxBool ((>=) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    LESS -> pure $ maybeToRight (InterpretError op "Invalid < operands") $ fmap LoxBool ((<) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    LESS_EQUAL -> pure $ maybeToRight (InterpretError op "Invalid <= operands") $ fmap LoxBool ((<=) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    BANG_EQUAL -> pure $ LoxBool <$> ((/=) <$> e1 <*> e2)
    EQUAL_EQUAL -> pure $ LoxBool <$> ((==) <$> e1 <*> e2)
    MINUS ->
      pure $ maybeToRight (InterpretError op "Invalid minus operands") $ fmap LoxNumber ((-) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    SLASH ->
      pure $ maybeToRight (InterpretError op "Invalid div operands") $ fmap LoxNumber ((/) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    STAR ->
      pure $ maybeToRight (InterpretError op "Invalid mult operands") $ fmap LoxNumber ((*) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
    PLUS ->
      pure $
        maybeToRight (InterpretError op "Invalid plus operands") $
          let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber
              addStrings = fmap LoxText $ Text.append <$> e1 ^? _Right . to stringify <*> e2 ^? _Right . to stringify
           in addNumbers <|> addStrings
    _ -> pure $ Left (InterpretError op "Invalid operand case")
  where
    opType = op ^. tokenType
--
interpret (ExprVariable name) = do
  let name' = name ^. lexeme . _Lexeme
  mv <- view $ to (envGet name')
  case mv of
    Just v -> pure $ Right v
    Nothing -> pure $ Left (InterpretError name [i|Undefined variable '#{name'}'|])

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

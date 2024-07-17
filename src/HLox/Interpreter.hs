module HLox.Interpreter (interpret, eval) where

import Control.Applicative ((<|>))
import Control.Lens.Combinators (to, _Right)
import Control.Lens.Operators ((^.), (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (for_)
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
evalPure = traverse executePure
  where
    executePure :: Stmt -> Either InterpretError LoxStmtValue
    executePure (StmtExpr e) = LoxStmtVoid <$ interpret e
    executePure (StmtPrint e) = LoxStmtPrint . stringify <$> interpret e

execute :: LoxStmtValue -> IO ()
execute LoxStmtVoid = pure ()
execute (LoxStmtPrint t) = TIO.putStrLn t

interpret :: Expr -> Either InterpretError LoxValue
--
interpret (ExprLiteral LitNothing) = Right LoxNil
interpret (ExprLiteral (LitText v)) = Right $ LoxText v
interpret (ExprLiteral (LitNumber v)) = Right $ LoxNumber v
interpret (ExprLiteral (LitBool v)) = Right $ LoxBool v
--
interpret (ExprGrouping e) = interpret e
--
interpret (ExprUnary t e) = case t ^. tokenType of
  BANG -> LoxBool . not . isTruthy <$> interpret e
  MINUS -> do
    right <- interpret e
    case right of
      LoxNumber n -> Right $ LoxNumber (negate n)
      _ -> Left $ InterpretError t "Unary minus on invalid value"
  _ -> Left $ InterpretError t "Unmatched case"
--
interpret (ExprBinary lhs op rhs) = case opType of
  GREATER -> maybeToRight (InterpretError op "Invalid operands") $ fmap LoxBool ((>) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  GREATER_EQUAL -> maybeToRight (InterpretError op "Invalid >= operands") $ fmap LoxBool ((>=) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  LESS -> maybeToRight (InterpretError op "Invalid < operands") $ fmap LoxBool ((<) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  LESS_EQUAL -> maybeToRight (InterpretError op "Invalid <= operands") $ fmap LoxBool ((<=) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  BANG_EQUAL -> LoxBool <$> ((/=) <$> e1 <*> e2)
  EQUAL_EQUAL -> LoxBool <$> ((==) <$> e1 <*> e2)
  MINUS ->
    maybeToRight (InterpretError op "Invalid minus operands") $ fmap LoxNumber ((-) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  SLASH ->
    maybeToRight (InterpretError op "Invalid div operands") $ fmap LoxNumber ((/) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  STAR ->
    maybeToRight (InterpretError op "Invalid mult operands") $ fmap LoxNumber ((*) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber)
  PLUS ->
    maybeToRight (InterpretError op "Invalid plus operands") $
      let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _Right . _LoxNumber <*> e2 ^? _Right . _LoxNumber
          addStrings = fmap LoxText $ Text.append <$> e1 ^? _Right . to stringify <*> e2 ^? _Right . to stringify
       in addNumbers <|> addStrings
  _ -> Left (InterpretError op "Invalid operand case")
  where
    e1 = interpret lhs
    e2 = interpret rhs
    opType = op ^. tokenType

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

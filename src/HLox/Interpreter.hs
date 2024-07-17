module HLox.Interpreter (interpret) where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((^.), (^?))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HLox.Interpreter.Types
import HLox.Parser.Types (Expr (..))
import HLox.Scanner.Types

interpret :: Expr -> LoxValue
--
interpret (ExprLiteral LitNothing) = LoxNil
interpret (ExprLiteral (LitText v)) = LoxText v
interpret (ExprLiteral (LitNumber v)) = LoxNumber v
interpret (ExprLiteral (LitBool v)) = LoxBool v
--
interpret (ExprGrouping e) = interpret e
--
interpret (ExprUnary t e) = case t ^. tokenType of
  BANG -> LoxBool $ not $ isTruthy right
  MINUS -> case right of
    LoxNumber n -> LoxNumber (negate n)
    _ -> error "Unary minus on invalid value"
  _ -> error "Unmatched case"
  where
    right = interpret e
--
interpret (ExprBinary lhs op rhs) = case opType of
  GREATER -> maybe (error "Invalid > operands") LoxBool ((>) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  GREATER_EQUAL -> maybe (error "Invalid >= operands") LoxBool ((>=) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  LESS -> maybe (error "Invalid < operands") LoxBool ((<) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  LESS_EQUAL -> maybe (error "Invalid <= operands") LoxBool ((<=) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  BANG_EQUAL -> LoxBool $ e1 /= e2
  EQUAL_EQUAL -> LoxBool $ e1 == e2
  MINUS ->
    maybe (error "Invalid minus operands") LoxNumber ((-) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  SLASH ->
    maybe (error "Invalid div operands") LoxNumber ((/) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  STAR ->
    maybe (error "Invalid mult operands") LoxNumber ((*) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
  PLUS ->
    fromMaybe (error "Invalid plus operands") $
      let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber
          addStrings = fmap LoxText $ Text.append <$> e1 ^? _LoxText <*> e2 ^? _LoxText
       in addNumbers <|> addStrings
  where
    e1 = interpret lhs
    e2 = interpret rhs
    opType = op ^. tokenType

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

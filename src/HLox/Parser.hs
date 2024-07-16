module HLox.Parser (pretty) where

import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Scanner.Types
import Control.Lens.Operators ((^.))
import HLox.Parser.Types
import Formatting (shortest, sformat)

pretty :: Expr -> Text
pretty (ExprBinary lhs op rhs) = [i|(#{l} #{pretty lhs} #{pretty rhs})|]
  where l = op ^. lexeme . _Lexeme
pretty (ExprGrouping e) = [i|(group #{pretty e})|]
pretty (ExprLiteral LitNothing) = "nil"
pretty (ExprLiteral (LitText txt)) = txt
pretty (ExprLiteral (LitNumber num)) = sformat shortest num
pretty (ExprUnary op e) = [i|(#{l} #{pretty e})|]
  where l = op ^. lexeme . _Lexeme

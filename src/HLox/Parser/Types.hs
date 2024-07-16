module HLox.Parser.Types where

import HLox.Scanner.Types

data Expr
  = ExprBinary !Expr !Token !Expr
  | ExprGrouping !Expr
  | ExprLiteral !Literal
  | ExprUnary !Token !Expr
  deriving (Show, Eq, Ord)

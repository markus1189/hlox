module HLox.Parser.Types where

import Control.Exception (Exception)
import Data.Text (Text)
import HLox.Scanner.Types

data Expr
  = ExprBinary !Expr !Token !Expr
  | ExprGrouping !Expr
  | ExprLiteral !Literal
  | ExprUnary !Token !Expr
  deriving (Show, Eq, Ord)

data Stmt = StmtExpr !Expr | StmtPrint !Expr deriving (Show, Eq, Ord)

data ParseError = ParseError !Token !Text deriving (Show, Eq)

instance Exception ParseError

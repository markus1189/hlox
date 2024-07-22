module HLox.Parser.Types where

import Control.Exception (Exception)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting (build, sformat)
import Formatting.Formatters (shortest)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types

data Expr
  = ExprAssign !Token !Expr
  | ExprBinary !Expr !Token !Expr
  | ExprLogical !Expr !Token !Expr
  | ExprCall !Expr !Token ![Expr]
  | ExprGrouping !Expr
  | ExprLiteral !Literal
  | ExprUnary !Token !Expr
  | ExprVariable !Token
  deriving (Show, Eq, Ord)

instance Pretty Expr where
  pretty :: Expr -> Text
  pretty (ExprBinary lhs op rhs) = [i|(#{pretty op} #{pretty lhs} #{pretty rhs})|]
  pretty (ExprLogical lhs op rhs) = [i|(#{pretty op} #{pretty lhs} #{pretty rhs})|]
  pretty (ExprGrouping e) = [i|(group #{pretty e})|]
  pretty (ExprLiteral LitNothing) = "nil"
  pretty (ExprLiteral (LitText txt)) = txt
  pretty (ExprLiteral (LitNumber num)) = sformat shortest num
  pretty (ExprLiteral (LitBool b)) = sformat build b
  pretty (ExprUnary op e) = [i|(#{pretty op} #{pretty e})|]
  pretty (ExprVariable t) = pretty t
  pretty (ExprAssign name v) = [i|(reassign #{pretty name} #{pretty v})|]
  pretty (ExprCall callee _ arguments) = [i|(call #{pretty callee} #{prettyList arguments})|]

prettyList :: Pretty a => [a] -> Text
prettyList [] = "()"
prettyList xs = [i|(list #{Text.unwords $ map pretty xs})|]

data Stmt
  = StmtExpr !Expr
  | StmtFunction !Token ![Token] ![Stmt]
  | StmtIf !Expr !Stmt !(Maybe Stmt)
  | StmtPrint !Expr
  | StmtVar !Token !(Maybe Expr)
  | StmtWhile !Expr !Stmt
  | StmtBlock ![Stmt]
  deriving (Show, Eq, Ord)

instance Pretty [Stmt] where
  pretty stmts = [i|(sequence #{Text.intercalate " " (map pretty' stmts)})|]
    where
      pretty' (StmtExpr e) = pretty e
      pretty' (StmtPrint e) = [i|(print #{pretty e})|]
      pretty' (StmtVar n e) = [i|(assign #{pretty n } #{maybe "" pretty e})|]
      pretty' (StmtBlock stmts') = [i|(block #{Text.unwords $ map pretty' stmts'})|]
      pretty' (StmtIf cond ifTrue ifFalse) = [i|(if #{pretty cond} #{pretty' ifTrue} #{maybe "" pretty' ifFalse})|]
      pretty' (StmtWhile cond body) = [i|(while #{pretty cond} #{pretty' body})|]
      pretty' (StmtFunction name params body) = [i|(declare-fun #{pretty name} #{prettyList params} #{pretty body})|]

data ParseError = ParseError !Token !Text deriving (Show, Eq)

instance Exception ParseError

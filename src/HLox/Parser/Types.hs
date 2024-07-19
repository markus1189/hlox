module HLox.Parser.Types where

import Control.Exception (Exception)
import Control.Lens.Combinators (view)
import Control.Lens.Operators ((^.))
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
  | ExprGrouping !Expr
  | ExprLiteral !Literal
  | ExprUnary !Token !Expr
  | ExprVariable !Token
  deriving (Show, Eq, Ord)

instance Pretty Expr where
  pretty :: Expr -> Text
  pretty (ExprBinary lhs op rhs) = [i|(#{l} #{pretty lhs} #{pretty rhs})|]
    where
      l = op ^. lexeme . _Lexeme
  pretty (ExprLogical lhs op rhs) = [i|(#{l} #{pretty lhs} #{pretty rhs})|]
    where
      l = op ^. lexeme . _Lexeme
  pretty (ExprGrouping e) = [i|(group #{pretty e})|]
  pretty (ExprLiteral LitNothing) = "nil"
  pretty (ExprLiteral (LitText txt)) = txt
  pretty (ExprLiteral (LitNumber num)) = sformat shortest num
  pretty (ExprLiteral (LitBool b)) = sformat build b
  pretty (ExprUnary op e) = [i|(#{l} #{pretty e})|]
    where
      l = op ^. lexeme . _Lexeme
  pretty (ExprVariable t) = t ^. lexeme . _Lexeme
  pretty (ExprAssign name v) = [i|(reassign #{name'}) #{pretty v}|]
    where
      name' = name ^. lexeme . _Lexeme

data Stmt
  = StmtExpr !Expr
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
      pretty' (StmtVar n e) = [i|(assign #{view (lexeme . _Lexeme) n } #{maybe "" pretty e})|]
      pretty' (StmtBlock stmts') = [i|(block #{Text.unwords $ map pretty' stmts'})|]
      pretty' (StmtIf cond ifTrue ifFalse) = [i|(if #{pretty cond} #{pretty' ifTrue} #{maybe "" pretty' ifFalse})|]
      pretty' (StmtWhile cond body) = [i|(while #{pretty cond} #{pretty' body})|]

data ParseError = ParseError !Token !Text deriving (Show, Eq)

instance Exception ParseError

module HLox.Parser.Types where

import Control.Exception (Exception)
import Control.Lens.Type (Lens')
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Formatting (build, sformat)
import Formatting.Formatters (shortest)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr
  = ExprAssign !UUID !Token !Expr
  | ExprBinary !UUID !Expr !Token !Expr
  | ExprLogical !UUID !Expr !Token !Expr
  | ExprCall !UUID !Expr !Token ![Expr]
  | ExprGet !UUID !Expr !Token
  | ExprSet !UUID !Expr !Token !Expr
  | ExprGrouping !UUID !Expr
  | ExprLiteral !UUID !Literal
  | ExprUnary !UUID !Token !Expr
  | ExprVariable !UUID !Token
  deriving (Show, Eq, Ord)

exprId :: Lens' Expr UUID
exprId f (ExprAssign uid name value) = fmap (\uid' -> ExprAssign uid' name value) (f uid)
exprId f (ExprBinary uid lhs op rhs) = fmap (\uid' -> ExprBinary uid' lhs op rhs) (f uid)
exprId f (ExprLogical uid lhs op rhs) = fmap (\uid' -> ExprLogical uid' lhs op rhs) (f uid)
exprId f (ExprCall uid callee paren arguments) = fmap (\uid' -> ExprCall uid' callee paren arguments) (f uid)
exprId f (ExprGet uid object name) = fmap (\uid' -> ExprGet uid' object name) (f uid)
exprId f (ExprSet uid object name value) = fmap (\uid' -> ExprSet uid' object name value) (f uid)
exprId f (ExprGrouping uid e) = fmap (`ExprGrouping` e) (f uid)
exprId f (ExprLiteral uid lit) = fmap (`ExprLiteral` lit) (f uid)
exprId f (ExprUnary uid op e) = fmap (\uid' -> ExprUnary uid' op e) (f uid)
exprId f (ExprVariable uid t) = fmap (`ExprVariable` t) (f uid)

instance Pretty Expr where
  pretty :: Expr -> Text
  pretty (ExprBinary _ lhs op rhs) = [i|(#{pretty op} #{pretty lhs} #{pretty rhs})|]
  pretty (ExprLogical _ lhs op rhs) = [i|(#{pretty op} #{pretty lhs} #{pretty rhs})|]
  pretty (ExprGrouping _ e) = [i|(group #{pretty e})|]
  pretty (ExprLiteral _ LitNothing) = "nil"
  pretty (ExprLiteral _ (LitText txt)) = txt
  pretty (ExprLiteral _ (LitNumber num)) = sformat shortest num
  pretty (ExprLiteral _ (LitBool b)) = sformat build b
  pretty (ExprUnary _ op e) = [i|(#{pretty op} #{pretty e})|]
  pretty (ExprVariable _ t) = pretty t
  pretty (ExprAssign _ name v) = [i|(reassign #{pretty name} #{pretty v})|]
  pretty (ExprCall _ callee _ arguments) = [i|(call #{pretty callee} #{prettyList arguments})|]
  pretty (ExprGet _ object name) = [i|(get #{pretty object} #{pretty name})|]
  pretty (ExprSet _ object name value) = [i|(set #{pretty object} #{pretty name} #{pretty value})|]

prettyList :: (Pretty a) => [a] -> Text
prettyList [] = "()"
prettyList xs = [i|(list #{Text.unwords $ map pretty xs})|]

data Stmt
  = StmtExpr !UUID !Expr
  | StmtFunction !StmtFunctionLit
  | StmtIf !UUID !Expr !Stmt !(Maybe Stmt)
  | StmtPrint !UUID !Expr
  | StmtReturn !UUID !Token !(Maybe Expr)
  | StmtVar !UUID !Token !(Maybe Expr)
  | StmtWhile !UUID !Expr !Stmt
  | StmtBlock !UUID ![Stmt]
  | StmtClass !UUID !Token !(Map Text StmtFunctionLit)
  deriving (Show, Eq, Ord)

data StmtFunctionLit = StmtFunctionLit !UUID !Token ![Token] ![Stmt] deriving (Show, Eq, Ord)

data ClassMethod = ClassMethod !UUID !Token ![Token] ![Stmt] deriving (Show, Eq, Ord)
instance Pretty ClassMethod where
  pretty (ClassMethod _ name params body) = [i|(method #{pretty name} #{prettyList params} #{pretty body})|]

instance Pretty [Stmt] where
  pretty stmts = [i|(sequence #{Text.intercalate " " (map pretty' stmts)})|]
    where
      pretty' (StmtExpr _ e) = pretty e
      pretty' (StmtPrint _ e) = [i|(print #{pretty e})|]
      pretty' (StmtVar _ n e) = [i|(assign #{pretty n } #{maybe "" pretty e})|]
      pretty' (StmtBlock _ stmts') = [i|(block #{Text.unwords $ map pretty' stmts'})|]
      pretty' (StmtIf _ cond ifTrue ifFalse) = [i|(if #{pretty cond} #{pretty' ifTrue} #{maybe "" pretty' ifFalse})|]
      pretty' (StmtWhile _ cond body) = [i|(while #{pretty cond} #{pretty' body})|]
      pretty' (StmtFunction lit) = pretty lit
      pretty' (StmtReturn _ keyword value) = [i|(#{pretty keyword} #{maybe "nil" pretty value})|]
      pretty' (StmtClass _ name methods) = [i|(class #{pretty name} (methods #{Text.unwords $ map pretty $ Map.elems methods}))|]

instance Pretty StmtFunctionLit where
  pretty (StmtFunctionLit _ name params body) = [i|(declare-fun #{pretty name} #{prettyList params} #{pretty body})|]

data ParseError = ParseError !Token !Text deriving (Show, Eq)

instance Exception ParseError

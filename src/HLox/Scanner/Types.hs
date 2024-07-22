module HLox.Scanner.Types where

import Control.Lens (makePrisms)
import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import HLox.Pretty (Pretty, pretty)
import Numeric.Natural (Natural)

newtype Line = Line Natural
  deriving (Show, Eq, Ord, Num)

makePrisms ''Line

newtype Lexeme = Lexeme Text
  deriving (Show, Eq, Ord)

makePrisms ''Lexeme

data Literal = LitNothing | LitText !Text | LitNumber !Double | LitBool !Bool
  deriving (Show, Eq, Ord)

makePrisms ''Literal

data TokenType
  = -- Single-character tokens.
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | -- One or two character tokens.
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals.
    IDENTIFIER
  | STRING
  | NUMBER
  | -- Keywords.
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | -- End of file.
    EOF
  deriving (Show, Eq, Ord)

data Token = Token
  { _tokenType :: !TokenType,
    _lexeme :: !Lexeme,
    _literal :: !Literal,
    _line :: !Line
  }
  deriving (Show, Eq, Ord)

makeLenses ''Token

instance Pretty Token where
  pretty (Token _ (Lexeme t) _ _) = t

data ScanError = ScanError !Line !Text deriving (Show, Eq, Ord)

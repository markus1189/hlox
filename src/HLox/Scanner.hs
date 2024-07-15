module HLox.Scanner (scanTokens) where

import Control.Lens (to, use, (%=), (.=))
import Control.Lens.Operators ((+=), (|>=), (<+~))
import Control.Lens.TH (makeLenses)
import Control.Monad.Loops (whileM_)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Class (MonadState)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Numeric.Natural (Natural)
import Control.Monad (when, void)
import Data.Char (isDigit)

newtype Line = Line Natural
  deriving (Show, Eq, Ord, Num)

newtype Lexeme = Lexeme Text
  deriving (Show, Eq, Ord)

data Literal = LitNothing | LitText Text | LitNumber Double
  deriving (Show, Eq, Ord)

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

data ScanError = ScanError Line Text

data ScanState = ScanState
  { _ssStart :: Int,
    _ssCurrent :: Int,
    _ssLine :: Line,
    _ssSource :: Text,
    _ssTokens :: [Token],
    _ssErrors :: [ScanError]
  }

makeLenses ''ScanState

scanTokens :: Text -> [Token]
scanTokens script = flip evalState (ScanState 0 0 (Line 1) script [] []) $ do
  whileM_ (not <$> scannerIsAtEnd) $ do
    current <- use ssCurrent
    ssStart .= current
    scanToken
  line <- use ssLine
  ssTokens %= (++ [Token EOF (Lexeme "") LitNothing line])
  use ssTokens

scanToken :: State ScanState ()
scanToken = do
  c <- advance
  case c of
    '(' -> addToken' LEFT_PAREN
    ')' -> addToken' RIGHT_PAREN
    '{' -> addToken' LEFT_BRACE
    '}' -> addToken' RIGHT_BRACE
    ',' -> addToken' COMMA
    '.' -> addToken' DOT
    '-' -> addToken' MINUS
    '+' -> addToken' PLUS
    ';' -> addToken' SEMICOLON
    '*' -> addToken' STAR
    '!' -> match' '=' BANG_EQUAL BANG >>= addToken'
    '=' -> match' '=' EQUAL_EQUAL EQUAL >>= addToken'
    '<' -> match' '=' LESS_EQUAL LESS >>= addToken'
    '>' -> match' '=' GREATER_EQUAL GREATER >>= addToken'
    '/' -> do
      b <- match '/'
      if b
        then do
          whileM_ ((&&) <$> ((/= '\n') <$> peek) <*> (not <$> scannerIsAtEnd)) advance
        else addToken' SLASH
    ' ' -> pure ()
    '\r' -> pure ()
    '\t' -> pure ()
    '\n' -> ssLine += 1
    '"' -> scanString
    (isDigit -> True) -> scanNumber
    _ -> do
      e <- ScanError <$> use ssLine <*> pure "Unexpected character."
      ssErrors |>= e
  where
    addToken' = addToken LitNothing
    match' c t f = match c >>= \b -> if b then pure t else pure f

addToken :: (MonadState ScanState m) => Literal -> TokenType -> m ()
addToken lit t = do
  source <- use ssSource
  start <- use ssStart
  current <- use ssCurrent
  let text = Lexeme $ slice start current source
  line <- use ssLine
  ssTokens |>= Token t text lit line

slice :: Int -> Int -> Text -> Text
slice start end = Text.take (end - start) . Text.drop start

advance :: (MonadState ScanState m) => m Char
advance = peek <* (ssCurrent += 1)

scannerIsAtEnd :: (MonadState ScanState m) => m Bool
scannerIsAtEnd = (>=) <$> use ssCurrent <*> use (ssSource . to Text.length)

match :: (MonadState ScanState m) => Char -> m Bool
match expected = do
  isAtEnd <- scannerIsAtEnd
  if isAtEnd
    then pure False
    else do
      c <- peek
      if c /= expected
        then pure False
        else do
          ssCurrent += 1
          pure True

peek :: (MonadState ScanState m) => m Char
peek = do
  isAtEnd <- scannerIsAtEnd
  if isAtEnd
    then pure '\0'
    else Text.index <$> use ssSource <*> use ssCurrent

scanString :: MonadState ScanState m => m ()
scanString = do
  let quote = (/= '"') <$> peek
  whileM_ ((&&) <$> quote <*> (not <$> scannerIsAtEnd)) $ do
    isNewline <- (== '\n') <$> peek
    when isNewline $ ssLine += 1
    advance
  isAtEnd <- scannerIsAtEnd
  if isAtEnd
    then do
      e <- ScanError <$> use ssLine <*> pure "Unterminated string."
      ssErrors |>= e
    else do
      -- closing '"'
      void advance
      value <- slice <$> ((+1) <$> use ssStart) <*> (subtract 1 <$> use ssCurrent) <*> use ssSource
      addToken (LitText value) STRING

scanNumber :: MonadState ScanState m => m ()
scanNumber = do
  whileM_ (isDigit <$> peek) advance
  dotThenDigit <- ((&&) <$> (('.' ==) <$> peek) <*> (isDigit <$> peekNext))
  when dotThenDigit $ do
    void advance
    whileM_ (isDigit <$> peek) advance

  value <- slice <$> use ssStart <*> use ssCurrent <*> use ssSource
  addToken (LitNumber $ unsafeFromRight $ Text.double value) NUMBER
  where unsafeFromRight (Right (x, _)) = x

peekNext :: MonadState ScanState m => m Char
peekNext = do
  isAtEnd <- (>=) <$> use (ssCurrent . to (+1)) <*> use (ssSource . to Text.length)
  if isAtEnd
    then pure '\0'
    else Text.index <$> use ssSource <*> use (ssCurrent . to (+1))

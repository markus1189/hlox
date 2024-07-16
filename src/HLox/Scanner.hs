module HLox.Scanner (scanTokens, Token (..), TokenType (..), Line, _Line, Lexeme, _Lexeme, Literal (..), ScanError (..)) where

import Control.Lens (over, to, use, view, (%=), (.=), _2)
import Control.Lens.Operators ((+=), (|>=))
import Control.Monad (replicateM_, void)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.Loops (whileM_)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (MonadState)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import HLox.Scanner.Types

reservedKeywords :: Map Text TokenType
reservedKeywords =
  Map.fromList
    [ ("and", AND),
      ("class", CLASS),
      ("else", ELSE),
      ("false", FALSE),
      ("for", FOR),
      ("fun", FUN),
      ("if", IF),
      ("nil", NIL),
      ("or", OR),
      ("print", PRINT),
      ("return", RETURN),
      ("super", SUPER),
      ("this", THIS),
      ("true", TRUE),
      ("var", VAR),
      ("while", WHILE)
    ]

scanTokens :: Text -> ([Token], [ScanError])
scanTokens script = over _2 (view ssErrors) $ flip runState (ScanState 0 0 (Line 1) script (Text.length script) [] []) $ do
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
      nextChar <- peek
      case nextChar of
        '/' -> whileM_ ((&&) <$> ((/= '\n') <$> peek) <*> (not <$> scannerIsAtEnd)) advance
        '*' -> do
          replicateM_ 2 advance
          scanBlockComment
        _ -> addToken' SLASH
    ' ' -> pure ()
    '\r' -> pure ()
    '\t' -> pure ()
    '\n' -> ssLine += 1
    '"' -> scanString
    (isDigit -> True) -> scanNumber
    (isAlpha -> True) -> scanIdentifier
    _ -> do
      e <- ScanError <$> use ssLine <*> pure "Unexpected character."
      ssErrors |>= e
  where
    addToken' = addToken LitNothing
    match' c t f = match c >>= \b -> if b then pure t else pure f

scanString :: (MonadState ScanState m) => m ()
scanString = do
  let quote = (/= '"') <$> peek
  whileM_ ((&&) <$> quote <*> (not <$> scannerIsAtEnd)) $ do
    whenM ((== '\n') <$> peek) $ ssLine += 1
    advance
  isAtEnd <- scannerIsAtEnd
  if isAtEnd
    then do
      e <- ScanError <$> use ssLine <*> pure "Unterminated string."
      ssErrors |>= e
    else do
      -- closing '"'
      void advance
      value <- slice <$> ((+ 1) <$> use ssStart) <*> (subtract 1 <$> use ssCurrent) <*> use ssSource
      addToken (LitText value) STRING

scanNumber :: (MonadState ScanState m) => m ()
scanNumber = do
  whileM_ (isDigit <$> peek) advance
  whenM ((&&) <$> (('.' ==) <$> peek) <*> (isDigit <$> peekNext)) $ do
    void advance
    whileM_ (isDigit <$> peek) advance

  value <- slice <$> use ssStart <*> use ssCurrent <*> use ssSource
  addToken (LitNumber $ unsafeFromRight $ Text.double value) NUMBER
  where
    unsafeFromRight (Right (x, _)) = x

scanIdentifier :: (MonadState ScanState m) => m ()
scanIdentifier = do
  whileM_ (isAlphaNumeric <$> peek) advance
  value <- slice <$> use ssStart <*> use ssCurrent <*> use ssSource
  let t = fromMaybe IDENTIFIER $ Map.lookup value reservedKeywords
  addToken LitNothing t

scanBlockComment :: (MonadState ScanState m) => m ()
scanBlockComment = do
  whileM_ ((&&) <$> ((/= '*') <$> peek) <*> (not <$> scannerIsAtEnd)) advance
  c1 <- peek
  c2 <- peekNext
  case [c1, c2] of
    "*/" -> do
      replicateM_ 2 advance
    _ -> do
      unlessM scannerIsAtEnd $ advance >> scanBlockComment

isAlpha :: Char -> Bool
isAlpha = flip elem $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

isAlphaNumeric :: Char -> Bool
isAlphaNumeric = (||) <$> isAlpha <*> isDigit

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
scannerIsAtEnd = (>=) <$> use ssCurrent <*> use ssSourceLength

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

peekNext :: (MonadState ScanState m) => m Char
peekNext = do
  isAtEnd <- (>=) <$> use (ssCurrent . to (+ 1)) <*> use ssSourceLength
  if isAtEnd
    then pure '\0'
    else Text.index <$> use ssSource <*> use (ssCurrent . to (+ 1))

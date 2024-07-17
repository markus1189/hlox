module HLox.Parser (pretty, parse) where

import Control.Exception (throwIO)
import Control.Lens.Combinators (preview, to, use, view, _last)
import Control.Lens.Operators ((+=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (void, when)
import Control.Monad.Catch (try)
import Control.Monad.Extra (andM, findM, ifM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (unfoldrM, whileM_)
import Control.Monad.Reader (lift)
import Control.Monad.State (MonadState, StateT (runStateT))
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Formatting (build, sformat, shortest)
import HLox.Parser.Types
import HLox.Scanner.Types
import HLox.Types (Lox)
import HLox.Util (loxReport)

data ParseState = ParseState
  { _psCurrent :: !Int,
    _psTokens :: Vector Token
  }
  deriving (Show)

makeLenses ''ParseState

pretty :: Expr -> Text
pretty (ExprBinary lhs op rhs) = [i|(#{l} #{pretty lhs} #{pretty rhs})|]
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

parse :: [Token] -> Lox (Either ParseError Expr)
parse tokens = try $ do
  fst <$> runStateT @_ @Lox parseExpression (ParseState 0 (Vector.fromList tokens))

parseExpression :: StateT ParseState Lox Expr
parseExpression = parseEquality

parseEquality :: StateT ParseState Lox Expr
parseEquality = parseBinary parseComparison [BANG_EQUAL, EQUAL_EQUAL]

parseComparison :: StateT ParseState Lox Expr
parseComparison = parseBinary parseTerm [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

parseTerm :: StateT ParseState Lox Expr
parseTerm = parseBinary parseFactor [MINUS, PLUS]

parseFactor :: StateT ParseState Lox Expr
parseFactor = parseBinary parseUnary [SLASH, STAR]

parseUnary :: StateT ParseState Lox Expr
parseUnary = do
  ifM
    (match [BANG, MINUS])
    ( do
        op <- previous
        ExprUnary op <$> parseUnary
    )
    parsePrimary

parsePrimary :: StateT ParseState Lox Expr
parsePrimary = do
  ifM (match [FALSE]) (pure $ ExprLiteral (LitBool False))
    $ ifM (match [TRUE]) (pure $ ExprLiteral (LitBool True))
    $ ifM (match [NIL]) (pure $ ExprLiteral LitNothing)
    $ ifM
      (match [NUMBER, STRING])
      ( do
          n <- previous
          pure $ ExprLiteral (n ^. literal)
      )
    $ ifM
      (match [LEFT_PAREN])
      ( do
          e <- parseExpression
          void $ consume RIGHT_PAREN "Expect ')' after expression."
          pure $ ExprGrouping e
      )
    $ do
      t <- peek
      err <- lift $ createError t "Expect expression."
      liftIO . throwIO $ err

parseBinary :: (MonadState ParseState m) => m Expr -> [TokenType] -> m Expr
parseBinary p ts = do
  expr <- p
  fromMaybe expr . preview _last <$> unfoldrM unfoldStep expr
  where
    unfoldStep e = do
      matches <- match ts
      if matches
        then do
          operator <- previous
          right <- p
          let r = ExprBinary e operator right
          pure $ Just (r, r)
        else pure Nothing

match :: (MonadState ParseState m) => [TokenType] -> m Bool
match types =
  isJust
    <$> findM
      ( \t -> do
          b <- check t
          when b (void advance)
          pure b
      )
      types

check :: (MonadState ParseState m) => TokenType -> m Bool
check tt = ifM isAtEnd (pure False) $ do
  current <- peek
  pure $ current ^. tokenType == tt

advance :: (MonadState ParseState m) => m Token
advance = do
  whenM (not <$> isAtEnd) (psCurrent += 1)
  previous

isAtEnd :: (MonadState ParseState m) => m Bool
isAtEnd = (== EOF) . view tokenType <$> peek

peek :: (MonadState ParseState m) => m Token
peek = (!) <$> use psTokens <*> use psCurrent

previous :: (MonadState ParseState m) => m Token
previous = (!) <$> use psTokens <*> use (psCurrent . to (subtract 1))

consume :: TokenType -> Text -> StateT ParseState Lox Token
consume tt msg = do
  ifM (check tt) advance $ do
    token <- peek
    err <- lift $ createError token msg
    liftIO . throwIO $ err

createError :: Token -> Text -> Lox ParseError
createError token msg = do
  if token ^. tokenType == EOF
    then loxReport (token ^. line) " at end" msg
    else loxReport (token ^. line) [i| at '#{token ^. lexeme}'|] msg
  pure $ ParseError token msg

synchronize :: StateT ParseState Lox ()
synchronize = do
  void advance
  whileM_
    ( andM
        [ not <$> isAtEnd,
          (== SEMICOLON) . view tokenType <$> previous,
          (`elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN]) . view tokenType <$> peek
        ]
    )
    advance

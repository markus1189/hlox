module HLox.Parser (Pretty (..), parse, parseExpr) where

import Control.Exception (throwIO)
import Control.Lens.Combinators (preview, to, use, view, _last)
import Control.Lens.Operators ((+=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (void, when)
import Control.Monad.Catch (catch, try)
import Control.Monad.Extra (andM, findM, ifM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (unfoldrM, whileM, whileM_)
import Control.Monad.Reader (lift)
import Control.Monad.State (MonadState, StateT (runStateT))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
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

class Pretty a where
  pretty :: a -> Text

instance Pretty Expr where
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
  pretty (ExprVariable t) = t ^. lexeme . _Lexeme
  pretty (ExprAssign name v) = [i|(reassign #{name'}) #{pretty v}|]
    where
      name' = name ^. lexeme . _Lexeme

instance Pretty [Stmt] where
  pretty stmts = [i|(sequence #{Text.intercalate " " (map pretty' stmts)})|]
    where
      pretty' (StmtExpr e) = pretty e
      pretty' (StmtPrint e) = [i|(print #{pretty e})|]
      pretty' (StmtVar n e) = [i|(assign #{view (lexeme . _Lexeme) n } #{maybe "" pretty e})|]

parseWith :: StateT ParseState Lox a -> [Token] -> Lox (Either ParseError a)
parseWith p tokens = try (fst <$> (runStateT @_ @Lox) p (ParseState 0 (Vector.fromList tokens)))

parse :: [Token] -> Lox (Either ParseError [Stmt])
parse = parseWith parseDeclarations

parseExpr :: [Token] -> Lox (Either ParseError Expr)
parseExpr = parseWith parseExpression

parseDeclarations :: StateT ParseState Lox [Stmt]
parseDeclarations = catMaybes <$> whileM (not <$> isAtEnd) parseDeclaration

parseDeclaration :: StateT ParseState Lox (Maybe Stmt)
parseDeclaration =
  ( Just
      <$> ifM
        (match [VAR])
        parseVarDeclaration
        parseStatement
  )
    `catch` \(ParseError _ _) -> Nothing <$ synchronize

parseVarDeclaration :: StateT ParseState Lox Stmt
parseVarDeclaration = do
  name <- consume IDENTIFIER "Expect variable name."
  initializer <- ifM (match [EQUAL]) (Just <$> parseExpression) (pure Nothing)
  void $ consume SEMICOLON "Expect ';' after variable declaration."
  pure $ StmtVar name initializer

parseStatement :: StateT ParseState Lox Stmt
parseStatement = ifM (match [PRINT]) parsePrintStatement parseExpressionStatement

parsePrintStatement :: StateT ParseState Lox Stmt
parsePrintStatement = do
  e <- parseExpression
  void $ consume SEMICOLON "Expect ';' after value."
  pure $ StmtPrint e

parseExpressionStatement :: StateT ParseState Lox Stmt
parseExpressionStatement = do
  e <- parseExpression
  void $ consume SEMICOLON "Expect ';' after expression."
  pure $ StmtExpr e

parseExpression :: StateT ParseState Lox Expr
parseExpression = parseAssignment

parseAssignment :: StateT ParseState Lox Expr
parseAssignment = do
  expr <- parseEquality

  ifM
    (match [EQUAL])
    ( do
        equals <- previous
        value <- parseAssignment

        case expr of
          ExprVariable name -> pure $ ExprAssign name value
          _ -> do
            lift $ void $ createError equals "Invalid assignment target."
            pure expr
    )
    (pure expr)

parseEquality :: StateT ParseState Lox Expr
parseEquality = parseBinary parseComparison [BANG_EQUAL, EQUAL_EQUAL]

parseComparison :: StateT ParseState Lox Expr
parseComparison = parseBinary parseTerm [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

parseTerm :: StateT ParseState Lox Expr
parseTerm = parseBinary parseFactor [MINUS, PLUS]

parseFactor :: StateT ParseState Lox Expr
parseFactor = parseBinary parseUnary [SLASH, STAR]

parseUnary :: StateT ParseState Lox Expr
parseUnary =
  ifM
    (match [BANG, MINUS])
    ( do
        op <- previous
        ExprUnary op <$> parseUnary
    )
    parsePrimary

parsePrimary :: StateT ParseState Lox Expr
parsePrimary = ifM (match [FALSE]) (pure $ ExprLiteral (LitBool False))
  $ ifM (match [TRUE]) (pure $ ExprLiteral (LitBool True))
  $ ifM (match [NIL]) (pure $ ExprLiteral LitNothing)
  $ ifM
    (match [NUMBER, STRING])
    ( do
        n <- previous
        pure $ ExprLiteral (n ^. literal)
    )
  $ ifM
    (match [IDENTIFIER])
    (ExprVariable <$> previous)
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
consume tt msg = ifM (check tt) advance $ do
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

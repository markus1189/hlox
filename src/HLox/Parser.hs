module HLox.Parser (Pretty (..), parse, parseExpr) where

import Control.Exception (throwIO)
import Control.Lens.Combinators (preview, to, use, view, _last)
import Control.Lens.Operators ((+=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (void, when)
import Control.Monad.Catch (catch, try)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (andM, findM, ifM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateM_, unfoldrM, untilM, whileM, whileM_)
import Control.Monad.Reader (lift)
import Control.Monad.State (MonadState, StateT (runStateT))
import Data.Either.Extra (fromEither)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import HLox.Parser.Types
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types
import HLox.Types (Lox, MonadFreshId, freshId)
import HLox.Util (loxReport)

data ParseState where
  ParseState ::
    {_psCurrent :: !Int, _psTokens :: !(Vector Token)} ->
    ParseState
  deriving (Show)

makeLenses ''ParseState

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
      <$> ( ifM (match [CLASS]) parseClassDeclaration
              $ ifM
                (match [FUN])
                (StmtFunction <$> parseFunction "function")
              $ ifM
                (match [VAR])
                parseVarDeclaration
                parseStatement
          )
  )
    `catch` \(ParseError _ _) -> Nothing <$ synchronize

parseClassDeclaration :: StateT ParseState Lox Stmt
parseClassDeclaration = do
  name <- consume IDENTIFIER "Expect class name."
  void $ consume LEFT_BRACE "Expect '{' before class body."
  methods <- whileM ((&&) <$> (not <$> check RIGHT_BRACE) <*> (not <$> isAtEnd)) (parseFunction "method")
  void $ consume RIGHT_BRACE "Expect '}' after class body."
  StmtClass <$> freshId <*> pure name <*> pure methods

parseFunction :: Text -> StateT ParseState Lox StmtFunctionLit
parseFunction kind = do
  name <- consume IDENTIFIER [i|Expect #{kind} name.|]
  void $ consume LEFT_PAREN [i|Expect '(' after #{kind} name.|]
  parameters <- ifM (check RIGHT_PAREN) (pure []) $ do
    consume IDENTIFIER "Expect parameter name." `untilM` (not <$> match [COMMA])
  when (length parameters >= 255) $ do
    t <- peek
    void . lift $ createError t "Can't have more than 255 arguments."
  void $ consume RIGHT_PAREN "Expect ')' after parameters."
  void $ consume LEFT_BRACE [i|Expect '{' before #{kind} body.|]
  (StmtFunctionLit <$> freshId <*> pure name <*> pure parameters <*> parseBlock)

parseVarDeclaration :: StateT ParseState Lox Stmt
parseVarDeclaration = do
  name <- consume IDENTIFIER "Expect variable name."
  initializer <- ifM (match [EQUAL]) (Just <$> parseExpression) (pure Nothing)
  void $ consume SEMICOLON "Expect ';' after variable declaration."
  StmtVar <$> freshId <*> pure name <*> pure initializer

parseStatement :: StateT ParseState Lox Stmt
parseStatement = do
  ifM (match [FOR]) parseForStatement $
    ifM (match [IF]) parseIfStatement $
      ifM (match [PRINT]) parsePrintStatement $
        ifM (match [RETURN]) parseReturnStatement $
          ifM (match [WHILE]) parseWhileStatement $
            ifM (match [LEFT_BRACE]) (StmtBlock <$> freshId <*> parseBlock) parseExpressionStatement

parseForStatement :: StateT ParseState Lox Stmt
parseForStatement = do
  void $ consume LEFT_PAREN "Expect '(' after 'for'."
  initializer <-
    ifM (match [SEMICOLON]) (pure Nothing) $
      ifM (match [VAR]) (Just <$> parseVarDeclaration) $
        Just <$> parseExpressionStatement
  condition <- ifM (not <$> check SEMICOLON) (Just <$> parseExpression) (pure Nothing)
  void $ consume SEMICOLON "Expect ';' after loop condition."
  increment <- ifM (not <$> check RIGHT_PAREN) (Just <$> parseExpression) (pure Nothing)
  void $ consume RIGHT_PAREN "Expect ')' after for clauses."
  body <- parseStatement
  body' <- case increment of
    Nothing -> pure body
    Just increment' -> do
      xs <- StmtExpr <$> freshId <*> pure increment'
      StmtBlock <$> freshId <*> pure [body, xs]
  condition' <- case condition of
    Nothing -> ExprLiteral <$> freshId <*> pure (LitBool True)
    Just c -> pure c
  body'' <- StmtWhile <$> freshId <*> pure condition' <*> pure body'
  case initializer of
    Nothing -> pure body''
    Just initializer' -> StmtBlock <$> freshId <*> pure [initializer', body'']

parseIfStatement :: StateT ParseState Lox Stmt
parseIfStatement = do
  void $ consume LEFT_PAREN "Expect '(' after 'if'."
  condition <- parseExpression
  void $ consume RIGHT_PAREN "Expect ')' after if condition."

  thenBranch <- parseStatement
  elseBranch <- ifM (match [ELSE]) (Just <$> parseStatement) (pure Nothing)

  StmtIf <$> freshId <*> pure condition <*> pure thenBranch <*> pure elseBranch

parsePrintStatement :: StateT ParseState Lox Stmt
parsePrintStatement = do
  e <- parseExpression
  void $ consume SEMICOLON "Expect ';' after value."
  StmtPrint <$> freshId <*> pure e

parseReturnStatement :: StateT ParseState Lox Stmt
parseReturnStatement = do
  keyword <- previous
  value <- ifM (check SEMICOLON) (pure Nothing) (Just <$> parseExpression)
  void $ consume SEMICOLON "Expect ';' after return value."
  StmtReturn <$> freshId <*> pure keyword <*> pure value

parseWhileStatement :: StateT ParseState Lox Stmt
parseWhileStatement = do
  condition <- consume LEFT_PAREN "Expect '(' after 'while'." *> parseExpression
  body <- consume RIGHT_PAREN "Expect ')' after 'while condition'." *> parseStatement
  StmtWhile <$> freshId <*> pure condition <*> pure body

parseBlock :: StateT ParseState Lox [Stmt]
parseBlock = do
  stmts <- catMaybes <$> whileM ((&&) <$> (not <$> check RIGHT_BRACE) <*> (not <$> isAtEnd)) parseDeclaration
  void $ consume RIGHT_BRACE "Expect '}' after block."
  pure stmts

parseExpressionStatement :: StateT ParseState Lox Stmt
parseExpressionStatement = do
  e <- parseExpression
  void $ consume SEMICOLON "Expect ';' after expression."
  StmtExpr <$> freshId <*> pure e

parseExpression :: StateT ParseState Lox Expr
parseExpression = parseAssignment

parseAssignment :: StateT ParseState Lox Expr
parseAssignment = do
  expr <- parseOr
  ifM
    (not <$> match [EQUAL])
    (pure expr)
    $ do
      equals <- previous
      value <- parseAssignment

      case expr of
        ExprVariable _ name -> lift $ ExprAssign <$> freshId <*> pure name <*> pure value
        ExprGet _ obj name -> ExprSet <$> freshId <*> pure obj <*> pure name <*> pure value
        _ -> do
          lift $ void $ createError equals "Invalid assignment target."
          pure expr

parseOr :: StateT ParseState Lox Expr
parseOr = parseBinary ExprLogical parseAnd [OR]

parseAnd :: StateT ParseState Lox Expr
parseAnd = parseBinary ExprLogical parseEquality [AND]

parseEquality :: StateT ParseState Lox Expr
parseEquality = parseBinary ExprBinary parseComparison [BANG_EQUAL, EQUAL_EQUAL]

parseComparison :: StateT ParseState Lox Expr
parseComparison = parseBinary ExprBinary parseTerm [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

parseTerm :: StateT ParseState Lox Expr
parseTerm = parseBinary ExprBinary parseFactor [MINUS, PLUS]

parseFactor :: StateT ParseState Lox Expr
parseFactor = parseBinary ExprBinary parseUnary [SLASH, STAR]

parseUnary :: StateT ParseState Lox Expr
parseUnary =
  ifM
    (match [BANG, MINUS])
    ( do
        op <- previous
        ExprUnary <$> freshId <*> pure op <*> parseUnary
    )
    parseCall

parseCall :: StateT ParseState Lox Expr
parseCall = do
  expr <- parsePrimary
  res <- runExceptT $ flip iterateM_ expr $ \expr' -> do
    ifM
      (match [LEFT_PAREN])
      (lift $ finishCall expr')
      $ ifM
        (match [DOT])
        ( lift $ do
            name <- consume IDENTIFIER "Expect property name after '.'."
            ExprGet <$> freshId <*> pure expr' <*> pure name
        )
      $ throwError expr'
  pure (fromEither res)

finishCall :: Expr -> StateT ParseState Lox Expr
finishCall callee = do
  arguments <- ifM (not <$> check RIGHT_PAREN) (parseExpression `untilM` (not <$> match [COMMA])) (pure [])
  paren <- consume RIGHT_PAREN "Expect ')' after arguments."
  when (length arguments >= 255) $ do
    t <- peek
    void $ lift $ createError t "Can't have more than 255 arguments."
  ExprCall <$> freshId <*> pure callee <*> pure paren <*> pure arguments

parsePrimary :: StateT ParseState Lox Expr
parsePrimary = ifM (match [FALSE]) (ExprLiteral <$> freshId <*> pure (LitBool False))
  $ ifM (match [TRUE]) (ExprLiteral <$> freshId <*> pure (LitBool True))
  $ ifM (match [NIL]) (ExprLiteral <$> freshId <*> pure LitNothing)
  $ ifM
    (match [NUMBER, STRING])
    ( do
        n <- previous
        ExprLiteral <$> freshId <*> pure (n ^. literal)
    )
  $ ifM
    (match [IDENTIFIER])
    (ExprVariable <$> freshId <*> previous)
  $ ifM
    (match [LEFT_PAREN])
    ( do
        e <- parseExpression
        void $ consume RIGHT_PAREN "Expect ')' after expression."
        ExprGrouping <$> freshId <*> pure e
    )
  $ do
    t <- peek
    err <- lift $ createError t "Expect expression."
    liftIO . throwIO $ err

parseBinary :: (MonadFreshId m, MonadState ParseState m) => (UUID -> a -> Token -> a -> a) -> m a -> [TokenType] -> m a
parseBinary ctor p ts = do
  expr <- p
  fromMaybe expr . preview _last <$> unfoldrM unfoldStep expr
  where
    unfoldStep e = do
      matches <- match ts
      if matches
        then do
          operator <- previous
          right <- p
          r <- ctor <$> freshId <*> pure e <*> pure operator <*> pure right
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

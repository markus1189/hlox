module HLox.Interpreter (eval, evalPure, evalExpr) where

import Data.HashTable.IO qualified as H
import Control.Applicative ((<|>))
import Control.Lens.Combinators (at, to, use, view)
import Control.Lens.Operators
  ( (%=),
    (.=),
    (<<.=),
    (^.),
    (^?),
  )
import Control.Monad (unless, void)
import Control.Monad.Error.Class (liftEither, tryError)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (whileM_)
import Control.Monad.RWS (MonadState, MonadWriter)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (UUID)
import HLox.Interpreter.Environment (assignAt, assignGlobal)
import HLox.Interpreter.Environment qualified as Env
import HLox.Interpreter.Types
import HLox.Parser.Types (Expr (..), Stmt (..), StmtFunctionLit (StmtFunctionLit))
import HLox.Resolver.Types
import HLox.Scanner.Types
import HLox.Types (Lox)
import HLox.Util (loxRuntimeError)

eval :: (Traversable t) => DepthMap -> Environment -> t Stmt -> Lox ()
eval dm env stmts = do
  r <- runExceptT $ runWriterT $ flip evalStateT (InterpreterState env dm) $ evalPure stmts
  case r of
    Left err -> loxRuntimeError err
    Right (_, effs) -> do liftIO $ for_ effs runEffect

evalExpr :: (MonadIO m) => DepthMap -> Environment -> Expr -> m (Either InterpretError LoxValue)
evalExpr dm env e = do
  r <- runExceptT $ runWriterT @[LoxEffect] $ flip evalStateT (InterpreterState env dm) $ interpret e
  pure $ fmap fst r

evalPure ::
  ( Traversable t,
    MonadIO m,
    HasDepthMap s DepthMap,
    HasEnvironment s Environment,
    MonadState s m,
    MonadError InterpretError m,
    MonadWriter [LoxEffect] m
  ) =>
  t Stmt ->
  m ()
evalPure = traverse_ executePure
  where
    executePure (StmtIf _ c t f) = do
      c' <- interpret c
      if isTruthy c'
        then executePure t
        else traverse_ executePure f
    executePure (StmtExpr _ e) = void $ interpret e
    executePure (StmtPrint _ e) = do
      x <- interpret e
      tell [LoxEffectPrint $ stringify x]
    executePure (StmtVar _ name Nothing) = do
      let name' = name ^. lexeme . _Lexeme
      env <- use environment
      Env.define env name' LoxNil
    executePure (StmtVar _ name (Just initializer)) = do
      v <- interpret initializer
      let name' = name ^. lexeme . _Lexeme
      env <- use environment
      Env.define env name' v
    executePure (StmtBlock _ stmts') = do
      env <- use environment >>= Env.pushEmpty
      environment .= env
      traverse_ executePure stmts'
      environment %= Env.pop
      pure ()
    executePure (StmtWhile _ cond body) = whileM_ (isTruthy <$> interpret cond) (executePure body)
    executePure (StmtFunction (StmtFunctionLit _ name params body)) = do
      env <- use environment
      let f = LoxFun (map (view (lexeme . _Lexeme)) params) env body
          name' = view (lexeme . _Lexeme) name
      Env.define env name' f
    executePure (StmtReturn _ _ value) = do
      r <- case value of
        Just v -> interpret v
        Nothing -> pure LoxNil
      throwError (InterpretReturn r)
    executePure (StmtClass _ name _) = do
      env <- use environment
      let name' = name ^. lexeme . _Lexeme
      Env.define env name' LoxNil
      let klass = LoxClass (Klass name')
      Env.assign env name name' klass

runEffect :: LoxEffect -> IO ()
runEffect (LoxEffectPrint t) = TIO.putStrLn t

interpret ::
  ( MonadIO m,
    HasDepthMap s DepthMap,
    HasEnvironment s Environment,
    MonadState s m,
    MonadError InterpretError m,
    MonadWriter [LoxEffect] m
  ) =>
  Expr ->
  m LoxValue
--
interpret (ExprLiteral _ LitNothing) = pure LoxNil
interpret (ExprLiteral _ (LitText v)) = pure $ LoxText v
interpret (ExprLiteral _ (LitNumber v)) = pure $ LoxNumber v
interpret (ExprLiteral _ (LitBool v)) = pure $ LoxBool v
--
interpret (ExprGrouping _ e) = interpret e
--
interpret (ExprUnary _ t e) = case t ^. tokenType of
  BANG -> LoxBool . not . isTruthy <$> interpret e
  MINUS -> do
    right <- interpret e
    case right of
      LoxNumber n -> pure $ LoxNumber (negate n)
      _ -> throwError $ InterpretRuntimeError t "Unary minus on invalid value"
  _ -> throwError $ InterpretRuntimeError t "Unmatched case"
--
interpret (ExprBinary _ lhs op rhs) = do
  e1 <- interpret lhs
  e2 <- interpret rhs
  case opType of
    BANG_EQUAL -> pure $ LoxBool $ e1 /= e2
    EQUAL_EQUAL -> pure $ LoxBool $ e1 == e2
    --
    GREATER -> applyBinaryBool (>) e1 e2 "Invalid > operands"
    GREATER_EQUAL -> applyBinaryBool (>=) e1 e2 "Invalid >= operands"
    LESS -> applyBinaryBool (<) e1 e2 "Invalid < operands"
    LESS_EQUAL -> applyBinaryBool (<=) e1 e2 "Invalid <= operands"
    --
    MINUS -> applyBinaryNumber (-) e1 e2 "Invalid minus operands"
    SLASH -> applyBinaryNumber (/) e1 e2 "Invalid div operands"
    STAR -> applyBinaryNumber (*) e1 e2 "Invalid mult operands"
    PLUS ->
      liftEither $
        maybeToRight (InterpretRuntimeError op "Invalid plus operands") $
          let addNumbers = fmap LoxNumber $ (+) <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber
              addStrings = fmap LoxText $ Text.append <$> e1 ^? to stringify <*> e2 ^? to stringify
           in addNumbers <|> addStrings
    _ -> throwError (InterpretRuntimeError op "Invalid operand case")
  where
    opType = op ^. tokenType
    applyBinaryNumber f e1 e2 msg = liftEither $ maybeToRight (InterpretRuntimeError op msg) $ fmap LoxNumber (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
    applyBinaryBool f e1 e2 msg = liftEither $ maybeToRight (InterpretRuntimeError op msg) $ fmap LoxBool (f <$> e1 ^? _LoxNumber <*> e2 ^? _LoxNumber)
--
interpret (ExprAssign eid name value) = do
  let name' = name ^. lexeme . _Lexeme
  v <- interpret value
  mDistance <- use (depthMap . _DepthMap . at eid)
  env <- use environment
  case mDistance of
    Just distance -> assignAt distance env name name' v
    Nothing -> assignGlobal env name name' v
  pure v
--
interpret (ExprVariable eid name) = lookupVariable name eid
--
interpret (ExprLogical _ lhs op rhs) = do
  left <- interpret lhs
  if op ^. tokenType == OR && isTruthy left || op ^. tokenType == AND && not (isTruthy left)
    then pure left
    else interpret rhs
--
interpret (ExprCall _ callee paren arguments) = do
  callee' <- interpret callee
  args <- traverse interpret arguments
  checkArity paren (length args) callee'
  let function = callee'
  call paren function args
interpret (ExprGet _ object name)   = do
  obj <- interpret object
  case obj of
    LoxInstance _ fs -> instanceGet name fs
    _ -> throwError $ InterpretRuntimeError name "Only instances have properties."
interpret (ExprSet _ object name value) = do
  obj <- interpret object
  case obj of
    LoxInstance _ fs -> do
      v <- interpret value
      instanceSet name v fs
      pure v
    _ -> throwError $ InterpretRuntimeError name "Only instances have fields."

instanceSet :: (MonadIO m) => Token -> LoxValue -> InstanceFields -> m ()
instanceSet (view (lexeme . _Lexeme) -> name) v (InstanceFields fields) = do
  liftIO $ H.insert fields name v

instanceGet :: (MonadError InterpretError m, MonadIO m) => Token -> InstanceFields -> m LoxValue
instanceGet name (InstanceFields fields) = do
  let name' = view (lexeme . _Lexeme) name
  r <- liftIO $ H.lookup fields name'
  case r of
    Nothing -> throwError $ InterpretRuntimeError name [i|Undefined property '#{name'}'.|]
    Just v -> pure v

lookupVariable ::
  ( MonadIO m,
    HasDepthMap s DepthMap,
    HasEnvironment s Environment,
    MonadState s m
  ) =>
  Token ->
  UUID ->
  m LoxValue
lookupVariable name eid = do
  mDistance <- use (depthMap . _DepthMap . at eid)
  case mDistance of
    Just distance -> do
      env <- use environment
      Env.unsafeLookupAt distance env (view (lexeme . _Lexeme) name)
    Nothing -> do
      env <- use environment
      Env.unsafeGlobalGet env (view (lexeme . _Lexeme) name)

call ::
  ( MonadIO m,
    MonadError InterpretError m,
    HasDepthMap s DepthMap,
    HasEnvironment s Environment,
    MonadState s m,
    MonadWriter [LoxEffect] m
  ) =>
  Token ->
  LoxValue ->
  [LoxValue] ->
  m LoxValue
call _ (LoxFun params funEnv body) args = do
  callEnv <- Env.pushEmpty funEnv
  for_ (params `zip` args) $ uncurry (Env.define callEnv)
  oldEnv <- environment <<.= callEnv
  r <- tryError $ evalPure body
  r' <- case r of
    Left (InterpretReturn v) -> pure $ Just v
    Left e@(InterpretRuntimeError _ _) -> throwError e
    Right _ -> pure Nothing
  environment .= oldEnv
  pure (fromMaybe LoxNil r')
call _ (LoxClass k) _ = LoxInstance k <$> liftIO (InstanceFields <$> H.new)
call _ (LoxNativeFun LoxClock) _ = LoxNumber . realToFrac <$> liftIO getPOSIXTime
call paren _ _ = throwError (InterpretRuntimeError paren "Can only call functions and classes.")

checkArity :: (MonadError InterpretError m) => Token -> Int -> LoxValue -> m ()
checkArity paren a1 (LoxFun a2 _ _) = unless (a1 == length a2) $ throwError (InterpretRuntimeError paren [i|Expected #{length a2} arguments but got #{a1}.|])
checkArity paren a1 (LoxNativeFun LoxClock) = unless (a1 == 0) $ throwError (InterpretRuntimeError paren [i|Expected 0 arguments but got #{a1}.|])
checkArity paren a1 (LoxClass _) = unless (a1 == 0) $ throwError (InterpretRuntimeError paren [i|Expected 0 arguments but got #{a1}.|])
checkArity _ _ _ = pure ()

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

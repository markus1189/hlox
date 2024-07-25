module HLox.Interpreter (eval, evalPure, evalExpr) where

import Control.Applicative ((<|>))
import Control.Lens.Combinators (at, to, use)
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
import Data.HashTable.IO qualified as H
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (UUID)
import HLox.Interpreter.Environment (assignAt, assignGlobal, unsafeLookupAt)
import HLox.Interpreter.Environment qualified as Env
import HLox.Interpreter.Types
import HLox.Parser.Types (Expr (..), Stmt (..), StmtFunctionLit (StmtFunctionLit), exprId, toName)
import HLox.Resolver.Types
import HLox.Scanner.Types
import HLox.Types (Lox)
import HLox.Util (loxRuntimeError)

eval :: (Traversable t) => DepthMap -> Environment -> t Stmt -> Lox ()
eval dm env stmts = do
  r <- runWriterT $ runExceptT $ flip evalStateT (InterpreterState env dm) $ evalPure stmts
  case r of
    (Left err, _) -> loxRuntimeError err
    (Right (), effs) -> do liftIO $ for_ effs runEffect

evalExpr :: (MonadIO m) => DepthMap -> Environment -> Expr -> m (Either InterpretError LoxValue)
evalExpr dm env e = do
  r <- runWriterT @[LoxEffect] $ runExceptT $ flip evalStateT (InterpreterState env dm) $ interpret e
  pure $ fst r

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
      env <- use environment
      Env.define env (toName name) LoxNil
    executePure (StmtVar _ name (Just initializer)) = do
      v <- interpret initializer
      env <- use environment
      Env.define env (toName name) v
    executePure (StmtBlock _ stmts') = do
      env <- use environment >>= Env.pushEmpty
      environment .= env
      traverse_ executePure stmts'
      environment %= Env.pop
      pure ()
    executePure (StmtWhile _ cond body) = whileM_ (isTruthy <$> interpret cond) (executePure body)
    executePure (StmtFunction (StmtFunctionLit _ name params body)) = do
      env <- use environment
      let f = LoxFun (LoxFunction (map toName params) env body LoxFunctionTypeRegular)
      Env.define env (toName name) f
    executePure (StmtReturn _ _ value) = do
      r <- case value of
        Just v -> interpret v
        Nothing -> pure LoxNil
      throwError (InterpretReturn r)
    executePure (StmtClass _ name ms) = do
      env <- use environment
      let name' = toName name
      Env.define env name' LoxNil
      let klass =
            LoxClass
              ( Klass
                  name'
                  ( ( KlassMethods
                        . fmap
                          ( \(StmtFunctionLit _ n ps body) ->
                              LoxFunction
                                (fmap toName ps)
                                env
                                body
                                ( if toName n == "init"
                                    then LoxFunctionTypeInitializer
                                    else LoxFunctionTypeRegular
                                )
                          )
                    )
                      ms
                  )
              )
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
  v <- interpret value
  mDistance <- use (depthMap . _DepthMap . at eid)
  env <- use environment
  case mDistance of
    Just distance -> assignAt distance env name (toName name) v
    Nothing -> assignGlobal env name (toName name) v
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
interpret (ExprGet _ object name) = do
  obj <- interpret object
  case obj of
    (LoxInst inst) -> instanceGet name inst
    _ -> throwError $ InterpretRuntimeError name "Only instances have properties."
interpret (ExprSet _ object name value) = do
  obj <- interpret object
  case obj of
    LoxInst (LoxInstance _ fs) -> do
      v <- interpret value
      instanceSet name v fs
      pure v
    _ -> throwError $ InterpretRuntimeError name "Only instances have fields."
interpret expr@(ExprThis _ n) = lookupVariable n (expr ^. exprId)

{-
---------------------------------------------------------------------------------------------------
-}

instanceSet :: (MonadIO m) => Token -> LoxValue -> InstanceFields -> m ()
instanceSet (toName -> name) v (InstanceFields fields) = do
  liftIO $ H.insert fields name v

instanceGet :: (MonadError InterpretError m, MonadIO m) => Token -> LoxInstance -> m LoxValue
instanceGet name inst@(LoxInstance klass (InstanceFields fields)) = do
  fld <- liftIO $ H.lookup fields (toName name)
  let mtd = lookupMethod (toName name) klass
  case fld of
    Just x -> pure x
    Nothing ->
      case mtd of
        Nothing -> throwError $ InterpretRuntimeError name [i|Undefined property '#{toName name}'.|]
        Just f -> LoxFun <$> functionBind f inst

functionBind :: (MonadIO m) => LoxFunction -> LoxInstance -> m LoxFunction
functionBind (LoxFunction n funEnv body t) inst = do
  callEnv <- Env.pushEmpty funEnv
  Env.define callEnv "this" (LoxInst inst)
  pure (LoxFunction n callEnv body t)

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
      Env.unsafeLookupAt distance env (toName name)
    Nothing -> do
      env <- use environment
      Env.unsafeGlobalGet env (toName name)

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
call _ (LoxFun (LoxFunction params funEnv body typ)) args = do
  callEnv <- Env.pushEmpty funEnv
  for_ (params `zip` args) $ uncurry (Env.define callEnv)
  oldEnv <- environment <<.= callEnv
  r <- tryError $ evalPure body
  r' <- case r of
    Left (InterpretReturn v) -> do
      if typ == LoxFunctionTypeInitializer
        then Env.lookupAt 0 funEnv "this"
        else pure $ Just v
    Left e@(InterpretRuntimeError _ _) -> throwError e
    Right () -> pure Nothing
  environment .= oldEnv
  case typ of
    LoxFunctionTypeInitializer -> unsafeLookupAt 0 funEnv "this"
    LoxFunctionTypeRegular -> pure (fromMaybe LoxNil r')
call paren (LoxClass k@(Klass _ (KlassMethods kms))) args = do
  inst <- LoxInstance k <$> liftIO (InstanceFields <$> H.new)
  let initializer = Map.lookup "init" kms
  for_ initializer $ \f -> do
    f' <- LoxFun <$> functionBind f inst
    call paren f' args
  pure (LoxInst inst)
call _ (LoxNativeFun LoxClock) _ = LoxNumber . realToFrac <$> liftIO getPOSIXTime
call paren _ _ = throwError (InterpretRuntimeError paren "Can only call functions and classes.")

checkArity :: (MonadError InterpretError m) => Token -> Int -> LoxValue -> m ()
checkArity paren a1 (LoxFun (LoxFunction a2 _ _ _)) = unless (a1 == length a2) $ throwError (InterpretRuntimeError paren [i|Expected #{length a2} arguments but got #{a1}.|])
checkArity paren a1 (LoxNativeFun LoxClock) = unless (a1 == 0) $ throwError (InterpretRuntimeError paren [i|Expected 0 arguments but got #{a1}.|])
checkArity paren a1 (LoxClass klass) = do
  let initializer = lookupMethod "init" klass
  case initializer of
    Just f -> checkArity paren a1 (LoxFun f)
    Nothing -> unless (a1 == 0) $ throwError (InterpretRuntimeError paren [i|Expected 0 arguments but got #{a1}.|])
checkArity _ _ _ = pure ()

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

lookupMethod :: Text -> Klass -> Maybe LoxFunction
lookupMethod n (Klass _ (KlassMethods m)) = Map.lookup n m

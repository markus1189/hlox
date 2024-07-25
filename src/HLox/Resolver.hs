module HLox.Resolver (resolve) where

import Control.Lens.Combinators
  ( at,
    ix,
    preuse,
    to,
    use,
    view,
    _head,
  )
import Control.Lens.Operators ((%=), (.=), (<<.=), (?=), (^.))
import Control.Monad (when)
import Control.Monad.Extra (ifM, whenM)
import Control.Monad.RWS.Strict (runRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Foldable (for_, traverse_)
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import HLox.Parser.Types
import HLox.Resolver.Types
import HLox.Scanner.Types

scopeEmpty :: ScopeStack -> Bool
scopeEmpty (ScopeStack []) = True
scopeEmpty (ScopeStack (_ : _)) = False

resolve :: (Foldable t) => t Stmt -> (DepthMap, [ResolverError])
resolve stmts = pick $ runRWS (resolveAll stmts) () (ResolverState (ScopeStack mempty) (DepthMap mempty) FunctionTypeNone ClassTypeNone)
  where
    pick (_, s, w) = (view depthMap s, w)

resolveAll :: (Foldable t, HasCurrentFunction s FunctionType, HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m, HasClassType s ClassType) => t Stmt -> m ()
resolveAll = traverse_ resolve1

resolve1 :: (HasCurrentFunction s FunctionType, HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m, HasClassType s ClassType) => Stmt -> m ()
resolve1 (StmtVar _ n initializer) = do
  declare n
  traverse_ resolveExpr initializer
  define n
resolve1 (StmtFunction f@(StmtFunctionLit _ name _ _)) = do
  declare name
  define name
  resolveFunction FunctionTypeFunction f
resolve1 (StmtExpr _ expr) = resolveExpr expr
resolve1 (StmtIf _ cond ifTrue ifFalse) = do
  resolveExpr cond
  resolve1 ifTrue
  traverse_ resolve1 ifFalse
resolve1 (StmtPrint _ expr) = resolveExpr expr
resolve1 (StmtReturn _ keyword expr) = do
  curFunc <- use currentFunction
  when (curFunc == FunctionTypeNone) . tell . pure $ ResolverError keyword "Can't return from top-level code."
  for_ expr $ \expr' -> do
    ifM
      ((== FunctionTypeIntializer) <$> use currentFunction)
      (tell . pure $ ResolverError keyword "Can't return a value from an initializer.")
      (resolveExpr expr')
resolve1 (StmtWhile _ c body) = resolveExpr c >> resolve1 body
resolve1 (StmtBlock _ stmts) = do
  beginScope
  resolveAll stmts
  endScope
resolve1 (StmtClass _ name methods mSuperclass) = do
  enclosingClass <- classType <<.= ClassTypeClass
  declare name
  define name

  for_ mSuperclass $ \superclass@(ExprVar _ t) -> do
    when (toName name == toName superclass) $ tell . pure $ ResolverError t "A class can't inherit from itself."
    resolveExpr (ExprVariable superclass)

  for_ mSuperclass $ const $ do
    beginScope
    scopeStack . _ScopeStack . _head . at "super" ?= Initialized

  beginScope
  scopeStack . _ScopeStack . _head . at "this" ?= Initialized
  for_ methods $ \m -> do
    let ft = if toName m == "init" then FunctionTypeIntializer else FunctionTypeMethod
    resolveFunction ft m
  endScope

  for_ mSuperclass $ const endScope

  classType .= enclosingClass

resolveFunction ::
  ( HasCurrentFunction s FunctionType,
    HasDepthMap s DepthMap,
    HasScopeStack s ScopeStack,
    MonadWriter [ResolverError] m,
    MonadState s m,
    HasClassType s ClassType
  ) =>
  FunctionType ->
  StmtFunctionLit ->
  m ()
resolveFunction ft (StmtFunctionLit _ _ params body) = do
  enclosingFunction <- currentFunction <<.= ft
  beginScope
  for_ params $ \param -> do
    declare param
    define param
  resolveAll body
  endScope
  currentFunction .= enclosingFunction

resolveExpr ::
  ( HasDepthMap s DepthMap,
    HasScopeStack s ScopeStack,
    MonadWriter [ResolverError] m,
    MonadState s m,
    HasClassType s ClassType
  ) =>
  Expr ->
  m ()
resolveExpr expr@(ExprVariable (ExprVar _ n)) = do
  scopesAreEmpty <- scopeEmpty <$> use scopeStack
  uninitialized <- (== Just Uninitialized) <$> preuse (scopeStack . _ScopeStack . _head . ix (toName n))
  if not scopesAreEmpty && uninitialized
    then tell [ResolverError n "Cannot read local variable in its own initializer"]
    else resolveLocal expr n
resolveExpr expr@(ExprAssign _ name value) = do
  resolveExpr value
  resolveLocal expr name
resolveExpr (ExprBinary _ lhs _ rhs) = resolveExpr lhs *> resolveExpr rhs
resolveExpr (ExprCall _ callee _ arguments) = do
  resolveExpr callee
  traverse_ resolveExpr arguments
resolveExpr (ExprGrouping _ expr) = resolveExpr expr
resolveExpr (ExprLiteral _ _) = pure ()
resolveExpr (ExprLogical _ lhs _ rhs) = resolveExpr lhs *> resolveExpr rhs
resolveExpr (ExprUnary _ _ expr) = resolveExpr expr
resolveExpr (ExprGet _ expr _) = resolveExpr expr
resolveExpr (ExprSet _ obj _ v) = resolveExpr v >> resolveExpr obj
resolveExpr expr@(ExprThis _ t) = do
  whenM ((== ClassTypeNone) <$> use classType) $ tell . pure $ ResolverError t "Can't use 'this' outside of a class."
  resolveLocal expr t
resolveExpr expr@(ExprSuper _ kw _) = resolveLocal expr kw

beginScope :: (HasScopeStack s ScopeStack, MonadState s m) => m ()
beginScope = scopeStack . _ScopeStack %= (mempty :)

endScope :: (HasScopeStack s ScopeStack, MonadState s m) => m ()
endScope =
  scopeStack . _ScopeStack %= \case
    [] -> []
    (_ : xs) -> xs

declare :: (MonadWriter [ResolverError] m, HasScopeStack s ScopeStack, MonadState s m) => Token -> m ()
declare n = do
  let n' = toName n
  scope <- use $ scopeStack . _ScopeStack . _head
  when (Map.member n' scope) . tell . pure $ ResolverError n "Already a variable with this name in this scope."
  scopeStack . _ScopeStack . _head . at n' ?= Uninitialized

define :: (HasScopeStack s ScopeStack, MonadState s m) => Token -> m ()
define n = scopeStack . _ScopeStack . _head . at (toName n) ?= Initialized

resolveLocal :: (HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => Expr -> Token -> m ()
resolveLocal expr name = do
  maybeDepth <- use (scopeStack . to (findDepth name))
  for_ maybeDepth $ \idx -> depthMap . _DepthMap %= Map.insert (expr ^. exprId) idx

findDepth :: Token -> ScopeStack -> Maybe Int
findDepth (toName -> name) (ScopeStack ss) = (\idx -> length ss - 1 - idx) <$> findIndex (Map.member name) (reverse ss)

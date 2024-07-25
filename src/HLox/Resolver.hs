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
resolve stmts = pick $ runRWS (resolveAll stmts) () (ResolverState (ScopeStack mempty) (DepthMap mempty) FunctionTypeNone)
  where
    pick (_, s, w) = (view depthMap s, w)

resolveAll :: (Foldable t, HasCurrentFunction s FunctionType, HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => t Stmt -> m ()
resolveAll = traverse_ resolve1

resolve1 :: (HasCurrentFunction s FunctionType, HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => Stmt -> m ()
resolve1 (StmtVar _ n initializer) = do
  declare n
  traverse_ resolveExpr initializer
  define n
resolve1 (StmtFunction _ name ps body) = do
  declare name
  define name
  resolveFunction ps body
resolve1 (StmtExpr _ expr) = resolveExpr expr
resolve1 (StmtIf _ cond ifTrue ifFalse) = do
  resolveExpr cond
  resolve1 ifTrue
  traverse_ resolve1 ifFalse
resolve1 (StmtPrint _ expr) = resolveExpr expr
resolve1 (StmtReturn _ keyword expr) = do
  curFunc <- use currentFunction
  when (curFunc == FunctionTypeNone) . tell . pure $ ResolverError keyword "Can't return from top-level code."
  traverse_ resolveExpr expr
resolve1 (StmtWhile _ c body) = resolveExpr c >> resolve1 body
resolve1 (StmtBlock _ stmts) = do
  beginScope
  resolveAll stmts
  endScope
resolve1 (StmtClass _ name _) = declare name >> define name

resolveFunction :: (HasCurrentFunction s FunctionType, HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => [Token] -> [Stmt] -> m ()
resolveFunction params body = do
  enclosingFunction <- currentFunction <<.= FunctionTypeFunction
  beginScope
  for_ params $ \param -> do
    declare param
    define param
  resolveAll body
  endScope
  currentFunction .= enclosingFunction

resolveExpr :: (HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => Expr -> m ()
resolveExpr expr@(ExprVariable _ n) = do
  scopesAreEmpty <- scopeEmpty <$> use scopeStack
  uninitialized <- (== Just Uninitialized) <$> preuse (scopeStack . _ScopeStack . _head . ix n')
  if not scopesAreEmpty && uninitialized
    then tell [ResolverError n "Cannot read local variable in its own initializer"]
    else resolveLocal expr n
  where
    n' = view (lexeme . _Lexeme) n
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

beginScope :: (HasScopeStack s ScopeStack, MonadState s m) => m ()
beginScope = scopeStack . _ScopeStack %= (mempty :)

endScope :: (HasScopeStack s ScopeStack, MonadState s m) => m ()
endScope =
  scopeStack . _ScopeStack %= \case
    [] -> []
    (_ : xs) -> xs

declare :: (MonadWriter [ResolverError] m, HasScopeStack s ScopeStack, MonadState s m) => Token -> m ()
declare n = do
  let n' = n ^. lexeme . _Lexeme
  scope <- use $ scopeStack . _ScopeStack . _head
  when (Map.member n' scope) . tell . pure $ ResolverError n "Already a variable with this name in this scope."
  scopeStack . _ScopeStack . _head . at n' ?= Uninitialized

define :: (HasScopeStack s ScopeStack, MonadState s m) => Token -> m ()
define n = scopeStack . _ScopeStack . _head . at (n ^. lexeme . _Lexeme) ?= Initialized

resolveLocal :: (HasDepthMap s DepthMap, HasScopeStack s ScopeStack, MonadWriter [ResolverError] m, MonadState s m) => Expr -> Token -> m ()
resolveLocal expr name = do
  maybeDepth <- use (scopeStack . to (findDepth name))
  for_ maybeDepth $ \idx -> depthMap . _DepthMap %= Map.insert (expr ^. exprId) idx

findDepth :: Token -> ScopeStack -> Maybe Int
findDepth (view (lexeme . _Lexeme) -> name) (ScopeStack ss) = (\idx -> length ss - 1 - idx) <$> findIndex (Map.member name) (reverse ss)

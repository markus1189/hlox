module HLox.Resolver (resolve) where

import Control.Lens (makePrisms, _Cons)
import Control.Lens.Combinators (at, view, _head, ix, use)
import Control.Lens.Cons (Cons)
import Control.Lens.Operators ((%=), (<&>), (?=), (^.))
import Control.Monad.Extra (andM, ifM, unlessM, whenM)
import Control.Monad.State.Class (MonadState)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Text (Text)
import HLox.Parser.Types
import HLox.Scanner.Types
import Control.Lens.Combinators (preuse)
import Control.Monad.Writer.Class (tell, MonadWriter)

newtype ScopeStack = ScopeStack [Map Text Bool] deriving (Show, Eq, Ord)

data ResolverError = ResolverError !Token !Text deriving (Show, Eq)

makePrisms ''ScopeStack

scopeEmpty :: ScopeStack -> Bool
scopeEmpty (ScopeStack []) = True
scopeEmpty (ScopeStack (_ : _)) = False

scopePeek :: ScopeStack -> Maybe (Map Text Bool)
scopePeek (ScopeStack []) = Nothing
scopePeek (ScopeStack (x : _)) = Just x

-- resolve :: (Foldable t, MonadState ScopeStack m) => t Stmt -> m _
resolve = traverse_ resolve1

resolve1 :: (MonadState ScopeStack m) => Stmt a -> m ()
resolve1 (StmtVar n initializer) = do
  declare n
  traverse_ resolveExpr initializer
  define n
resolve1 _ = pure ()

resolveExpr :: (MonadWriter [ResolverError] m, MonadState ScopeStack m) => Expr -> m ()
resolveExpr expr@(ExprVariable n) = do
  scopesAreEmpty <- scopeEmpty <$> use id
  uninitialized <- (== Just False) <$> preuse (_ScopeStack . _head . ix n')
  if not scopesAreEmpty && uninitialized
    then tell [ResolverError n "Cannot read local variable in its own initializer"]
    else resolveLocal expr n
  where
    n' = view (lexeme . _Lexeme) n
resolveExpr _ = pure ()

beginScope :: (MonadState ScopeStack m) => m ()
beginScope = _ScopeStack %= (mempty :)

endScope :: (MonadState ScopeStack m) => m ()
endScope =
  _ScopeStack %= \case
    [] -> []
    (_ : xs) -> xs

declare :: (MonadState ScopeStack m) => Token -> m ()
declare n = _ScopeStack . _head . at (n ^. lexeme . _Lexeme) ?= False

define :: (MonadState ScopeStack m) => Token -> m ()
define n = _ScopeStack . _head . at (n ^. lexeme . _Lexeme) ?= True

resolveLocal :: (MonadWriter [ResolverError] m, MonadState ScopeStack m) => Expr -> Token -> m ()
resolveLocal expr name = do
  pure ()
  where name' = view (lexeme . _Lexeme) name

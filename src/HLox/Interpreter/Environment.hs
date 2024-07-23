module HLox.Interpreter.Environment (global, pushEmpty, pop, define, lookup, assign, size) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashTable.IO qualified as H
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Interpreter.Types (Environment (..), InterpretError (..), LoxNativeFunKind (..), LoxValue (..))
import HLox.Scanner.Types (Token)
import Prelude hiding (lookup)

global :: (MonadIO m) => m Environment
global = Environment . pure <$> liftIO (H.fromList [("clock", LoxNativeFun LoxClock)])

pushEmpty :: (MonadIO m) => Environment -> m Environment
pushEmpty (Environment es) = Environment . (<| es) <$> liftIO H.new

pop :: Environment -> Environment
pop (Environment (e :| [])) = Environment (e :| [])
pop (Environment (_ :| (e : es))) = Environment (e :| es)

define :: (MonadIO m) => Environment -> Text -> LoxValue -> m ()
define (Environment (e :| _)) k v = liftIO $ H.insert e k v

lookup :: (MonadIO m) => Environment -> Text -> m (Maybe LoxValue)
lookup (Environment es) k = liftIO . go . NonEmpty.toList $ es
  where
    go [] = pure Nothing
    go (e : es') = (<|>) <$> H.lookup e k <*> go es'

assign :: (MonadError InterpretError m, MonadIO m) => Environment -> Token -> Text -> LoxValue -> m ()
assign (Environment es) t k v = go . NonEmpty.toList $ es
  where
    go [] = throwError $ InterpretRuntimeError t [i|Undefined variable '#{k}'|]
    go (e : es') =
      ifM
        (isJust <$> liftIO (H.lookup e k))
        (liftIO $ void (liftIO (H.insert e k v)))
        (go es')

size :: Environment -> Int
size (Environment es) = NonEmpty.length es

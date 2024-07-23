{-# LANGUAGE AllowAmbiguousTypes #-}
module HLox.Interpreter.Types where

import Control.Lens.TH (makePrisms)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Parser.Types (Stmt)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types ( Token )

import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Extra (ifM)
import Data.Maybe (isJust)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Applicative ((<|>))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad (void)

type HashTable k v = H.CuckooHashTable k v

data LoxNativeFunKind = LoxClock deriving (Show, Eq, Ord)

data LoxEffect = LoxEffectPrint !Text
  deriving (Show, Eq, Ord)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  | LoxFun ![Text] !Environment ![Stmt]
  | LoxNativeFun !LoxNativeFunKind
  deriving (Show, Eq, Ord)

instance Pretty LoxValue where
  pretty = stringify

stringify :: LoxValue -> Text
stringify LoxNil = "nil"
stringify (LoxNumber n) = sformat shortest n
stringify (LoxText t) = t
stringify (LoxBool True) = "true"
stringify (LoxBool False) = "false"
stringify (LoxFun _ _ _) = "<function>"
stringify (LoxNativeFun k) = [i|<native function: #{show k}>|]

data InterpretError
  = InterpretRuntimeError !Token !Text
  | InterpretReturn !LoxValue
  deriving (Show, Eq)

newtype Environment = Environment (NonEmpty (HashTable Text LoxValue)) deriving (Show)

instance Eq Environment where
  (==) _ _ = False

instance Ord Environment where
  compare _ _ = EQ

makePrisms ''LoxValue
makePrisms ''Environment

initialEnv :: MonadIO m => m Environment
initialEnv = Environment . pure <$> liftIO (H.fromList [("clock", LoxNativeFun LoxClock)])

pushEmptyEnv :: MonadIO m => Environment -> m Environment
pushEmptyEnv (Environment es) = Environment . (<| es) <$> liftIO H.new

popEnv :: Environment -> Environment
popEnv (Environment (e:|[])) = Environment (e:|[])
popEnv (Environment (_:|(e:es))) = Environment (e :| es)

envDefine :: MonadIO m => Environment -> Text -> LoxValue -> m ()
envDefine (Environment (e :| _)) k v = liftIO $ H.insert e k v

envLookup :: MonadIO m => Environment -> Text -> m (Maybe LoxValue)
envLookup (Environment es) k = liftIO . go . NonEmpty.toList $ es
  where
    go [] = pure Nothing
    go (e:es') = (<|>) <$> H.lookup e k <*> go es'

envAssign :: (MonadError InterpretError m, MonadIO m) => Environment -> Token -> Text -> LoxValue -> m ()
envAssign (Environment es) t k v = go . NonEmpty.toList $ es
  where
    go [] = throwError $ InterpretRuntimeError t [i|Undefined variable '#{k}'|]
    go (e:es') = ifM (isJust <$> liftIO (H.lookup e k))
      (liftIO $ void (liftIO (H.insert e k v)))
      (go es')

envSize :: Environment -> Int
envSize (Environment es) = NonEmpty.length es

module HLox.Types where

import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, newIORef)

data LoxEnv = LoxEnv
  { _hadError :: IORef Bool
  , _hadRuntimeError :: IORef Bool
  }

makeLenses ''LoxEnv

makeLoxEnv :: IO LoxEnv
makeLoxEnv = LoxEnv <$> newIORef False <*> newIORef False

newtype Lox a = Lox (ReaderT LoxEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow
    )

runLox :: Lox a -> LoxEnv -> IO a
runLox (Lox a) = runReaderT a

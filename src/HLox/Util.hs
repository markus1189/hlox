module HLox.Util
  ( loxHadError,
    loxWriteHadError,
    loxError,
    loxReport,
    loxRuntimeError,
    loxHadRuntimeError,
  )
where

import Control.Lens.Combinators (view)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import HLox.Interpreter.Types (InterpretError (InterpretRuntimeError))
import HLox.Scanner.Types (Line, line)
import HLox.Types (Lox (Lox), hadError, hadRuntimeError)
import System.IO (stderr)

loxError :: Line -> Text -> Lox ()
loxError l message = do
  loxReport l "" message

loxRuntimeError :: InterpretError -> Lox ()
loxRuntimeError (InterpretRuntimeError t msg) = do
  liftIO $ TIO.hPutStrLn stderr [i|#{msg}\n[line #{t ^. line}]|]
  e <- Lox $ view hadRuntimeError
  liftIO $ writeIORef e True
loxRuntimeError _ = pure ()

loxReport :: Line -> Text -> Text -> Lox ()
loxReport l loc message = do
  liftIO $ TIO.hPutStrLn stderr [i|[line #{l}] Error #{loc}: #{message}|]
  loxWriteHadError True

loxHadError :: Lox Bool
loxHadError = Lox $ do
  e <- view hadError
  liftIO $ readIORef e

loxHadRuntimeError :: Lox Bool
loxHadRuntimeError = Lox $ do
  e <- view hadRuntimeError
  liftIO $ readIORef e

loxWriteHadError :: Bool -> Lox ()
loxWriteHadError b = Lox $ do
  e <- view hadError
  liftIO $ writeIORef e b

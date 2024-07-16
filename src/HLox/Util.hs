module HLox.Util (loxHadError, loxWriteHadError, loxError, loxReport) where

import Control.Lens.Combinators (view)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Scanner.Types (Line)
import HLox.Types (Lox (Lox), hadError)
import System.IO (stderr)

loxError :: Line -> Text -> Lox ()
loxError line message = do
  loxReport line "" message

loxReport :: Line -> Text -> Text -> Lox ()
loxReport line loc message = do
  liftIO $ TIO.hPutStrLn stderr $ "[line " <> Text.pack (show line) <> "] Error" <> loc <> ": " <> message
  loxWriteHadError True

loxHadError :: Lox Bool
loxHadError = Lox $ do
  e <- view hadError
  liftIO $ readIORef e

loxWriteHadError :: Bool -> Lox ()
loxWriteHadError b = Lox $ do
  e <- view hadError
  liftIO $ writeIORef e b

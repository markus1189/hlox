module HLox (main) where

import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Streaming.Prelude qualified as S
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

import HLox.Scanner (scanTokens)

data LoxEnv = LoxEnv
  { _hadError :: IORef Bool
  }

makeLenses ''LoxEnv

makeLoxEnv :: IO LoxEnv
makeLoxEnv = LoxEnv <$> newIORef False

newtype Lox a = Lox (ReaderT LoxEnv IO a) deriving (Functor, Applicative, Monad, MonadIO)

runLox :: LoxEnv -> Lox a -> IO a
runLox e (Lox m) = do
  runReaderT m e

loxHadError :: Lox Bool
loxHadError = Lox $ do
  e <- view hadError
  liftIO $ readIORef e

loxWriteHadError :: Bool -> Lox ()
loxWriteHadError b = Lox $ do
  e <- view hadError
  liftIO $ writeIORef e b

main :: IO ()
main = do
  env <- makeLoxEnv
  TIO.putStrLn "Welcome to the haskell Lox implementation"
  args <- getArgs
  if length args > 1
    then do
      putStrLn "Usage: jlox [script]"
      exitWith (ExitFailure 64)
    else
      runLox env $
        if length args == 1
          then runFile $ head args
          else runPrompt

runFile :: FilePath -> Lox ()
runFile fp = do
  script <- liftIO $ TIO.readFile fp
  run script
  err <- loxHadError
  when err $ liftIO $ exitWith (ExitFailure 65)

runPrompt :: Lox ()
runPrompt = do
  S.effects $
    S.mapM mapper $
      S.takeWhile (not . Text.isPrefixOf ":q") $
        S.map Text.pack $
          S.stdinLn @Lox
  liftIO $ TIO.putStrLn "Goodbye"
  where
    mapper :: Text -> Lox ()
    mapper line = do
      run line
      loxWriteHadError False

run :: Text -> Lox ()
run script = do
  let (tokens,errs) = scanTokens script
  unless (null errs) $
    liftIO $ mapM_ (TIO.hPutStrLn stderr . Text.pack . show) errs
  liftIO $ print $ tokens

loxError :: Int -> Text -> Lox ()
loxError line message = do
  report line "" message

report :: Int -> Text -> Text -> Lox ()
report line loc message = liftIO $ do
  TIO.hPutStrLn stderr $ "[line " <> Text.pack (show line) <> "] Error" <> loc <> ": " <> message
  exitWith (ExitFailure line)

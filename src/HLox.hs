module HLox (main) where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Interpreter (interpret)
import HLox.Interpreter.Types (stringify)
import HLox.Parser (parse)
import HLox.Scanner (scanTokens)
import HLox.Types (Lox, makeLoxEnv, runLox)
import HLox.Util
import Streaming.Prelude qualified as S
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

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
      flip runLox env $
        if length args == 1
          then runFile $ head args
          else runPrompt

runFile :: FilePath -> Lox ()
runFile fp = do
  script <- liftIO $ TIO.readFile fp
  run script
  whenM loxHadError $ liftIO $ exitWith (ExitFailure 65)
  whenM loxHadRuntimeError $ liftIO $ exitWith (ExitFailure 70)

runPrompt :: Lox ()
runPrompt = do
  S.effects $
    S.mapM mapper $
      S.takeWhile (not . Text.isPrefixOf ":q") $
        S.map convertString $
          S.stdinLn @Lox
  liftIO $ TIO.putStrLn "Goodbye"
  where
    mapper :: Text -> Lox ()
    mapper line = do
      run line
      loxWriteHadError False

run :: Text -> Lox ()
run script = do
  let (tokens, errs) = scanTokens script
  parseResult <- parse tokens
  case parseResult of
    Left _ -> pure ()
    Right expr -> do
      let result = interpret expr
      case result of
        Left err -> do
          loxRuntimeError err
        Right val -> do
          liftIO $ TIO.putStrLn $ stringify val

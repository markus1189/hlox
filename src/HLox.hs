module HLox (main) where

import Control.Lens (view)
import Control.Monad.Except (runExceptT)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Foldable (for_, traverse_)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Interpreter (eval, interpret)
import HLox.Parser (parse, parseExpr, pretty)
import HLox.Scanner (TokenType (SEMICOLON), scanTokens)
import HLox.Scanner.Types (ScanError (..), tokenType)
import HLox.Types (Lox, makeLoxEnv, runLox)
import HLox.Util
import Streaming.Prelude qualified as S
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified HLox.Interpreter.Environment as Env

main :: IO ()
main = do
  env <- makeLoxEnv
  TIO.putStrLn "Welcome to the haskell Lox implementation"
  args <- getArgs
  if length args > 1
    then do
      putStrLn "Usage: hlox [script]"
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
  case errs of
    [] -> do
      let isStatement = SEMICOLON `elem` map (view tokenType) tokens
      if isStatement
        then do
          parseResult <- parse tokens
          case parseResult of
            Left _ -> pure ()
            Right stmts -> do
              env <- Env.global
              eval env stmts
        else do
          parseResult <- parseExpr tokens
          for_ parseResult $ \stmts -> do
            env <- Env.global
            result <- fmap (fmap fst) $ runExceptT $ runWriterT $ flip evalStateT env (interpret stmts)
            traverse_ (liftIO . TIO.putStrLn . pretty) result
    scanErrs -> do
      liftIO $ TIO.putStrLn "There were scan errors:"
      for_ scanErrs $ \(ScanError l msg) -> loxError l msg

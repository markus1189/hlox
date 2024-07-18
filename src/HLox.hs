module HLox (main) where

import Control.Lens (view)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT)
import Data.Foldable (for_, traverse_)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Interpreter (eval, interpret)
import HLox.Interpreter.Types (Environment, InterpretError)
import HLox.Parser (parse, parseExpr, pretty)
import HLox.Parser.Types (Stmt (StmtPrint))
import HLox.Scanner (TokenType (SEMICOLON), scanTokens)
import HLox.Scanner.Types (ScanError (..), tokenType)
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
        S.map convertString $ do
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
            Right stmts -> eval stmts
        else do
          parseResult <- parseExpr tokens
          case parseResult of
            Left _ -> pure ()
            Right stmts -> do
              let x = evalStateT (interpret @(StateT Environment (Either InterpretError)) stmts) mempty
              liftIO $ traverse_ TIO.putStrLn $ fmap pretty $ x
    scanErrs -> do
      liftIO $ TIO.putStrLn "There were scan errors:"
      for_ scanErrs $ \(ScanError l msg) -> loxError l msg

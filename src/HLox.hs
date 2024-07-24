module HLox (main) where

import Control.Lens (view)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import HLox.Interpreter (eval, evalExpr)
import HLox.Interpreter.Environment qualified as Env
import HLox.Interpreter.Types (InterpretError (..))
import HLox.Parser (parse, parseExpr, pretty)
import HLox.Resolver (resolve)
import HLox.Resolver.Types (DepthMap (DepthMap), ResolverError (..))
import HLox.Scanner (TokenType (SEMICOLON), scanTokens)
import HLox.Scanner.Types (ScanError (..), line, tokenType)
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
        S.map convertString $
          S.stdinLn @Lox
  liftIO $ TIO.putStrLn "Goodbye"
  where
    mapper :: Text -> Lox ()
    mapper l = do
      run l
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
              let (dm, errors) = resolve stmts
              if not $ null errors
                then do
                  liftIO $ TIO.putStrLn "There were resolver errors:"
                  for_ errors $ \(ResolverError t msg) -> loxError (view line t) msg
                else do
                  env <- Env.global
                  eval dm env stmts
        else do
          parseResult <- parseExpr tokens
          for_ parseResult $ \expr -> do
            let dm = DepthMap mempty
            env <- Env.global
            result <- evalExpr dm env expr
            case result of
              Left (InterpretRuntimeError (view line -> l) msg) -> loxError l msg
              Left (InterpretReturn _) -> loxError (-1) "Invalid return while evaluating expression."
              Right v -> liftIO . TIO.putStrLn . pretty $ v
    scanErrs -> do
      liftIO $ TIO.putStrLn "There were scan errors:"
      for_ scanErrs $ \(ScanError l msg) -> loxError l msg

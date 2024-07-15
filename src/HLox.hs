{-# LANGUAGE OverloadedStrings #-}

module HLox (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Streaming.Prelude qualified as S
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

data Token = Token !Text
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the haskell Lox implementation"
  args <- getArgs
  if length args > 1
    then do
      putStrLn "Usage: jlox [script]"
      exitWith (ExitFailure 64)
    else
      if length args == 1
        then runFile $ head args
        else runPrompt

runFile :: FilePath -> IO ()
runFile fp = do
  script <- TIO.readFile fp
  run script

runPrompt :: IO ()
runPrompt = do
  S.effects $
    S.mapM run $
      S.takeWhile (not . Text.isPrefixOf ":q") $
        S.map Text.pack $
          S.stdinLn @IO
  TIO.putStrLn "Goodbye"

run :: Text -> IO ()
run script = do
  print (scanTokens script)

scanTokens :: Text -> [Token]
scanTokens = map Token . Text.words

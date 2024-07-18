module HLox.Interpreter.Test where

import Control.Monad.State (evalState)
import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Interpreter (evalPure, interpret)
import HLox.Interpreter.Types
import HLox.Parser (parse, parseExpr)
import HLox.Scanner (scanTokens)
import HLox.Types
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec_interpreterExpr :: SpecWith ()
spec_interpreterExpr = do
  describe "Interpreter" $ do
    describe "valid expressions" $ do
      it "should evaluate boolean expression" $ do
        Right result <- interpretExpr' "1 + 5 < 3 * 3"
        result `shouldBe` LoxBool True
      it "should evaluate number expression " $ do
        Right result <- interpretExpr' "42 * 2"
        result `shouldBe` LoxNumber 84
      it "should concatenate strings" $ do
        Right result <- interpretExpr' [i|"Hello" + " " + "World"|]
        result `shouldBe` LoxText "Hello World"
      it "should stringify lhs operands" $ do
        Right result <- interpretExpr' [i|42 + "!"|]
        result `shouldBe` LoxText "42!"
      it "should stringify rhs operands" $ do
        Right result <- interpretExpr' [i|"Answer: " + 42|]
        result `shouldBe` LoxText "Answer: 42"

spec_interpreterStmt :: SpecWith ()
spec_interpreterStmt = do
  describe "Interpreter" $ do
    describe "valid statements" $ do
      it "should handle variables" $ do
        Right result <- interpretStmt' "var a = 1; var b = 2; print a + b;"
        result `shouldBe` [LoxStmtVoid, LoxStmtVoid, LoxStmtPrint "3"]
      it "should handle re-assignments" $ do
        Right result <- interpretStmt' "var a = 1; var b = 2; a = 2; print a + b;"
        result `shouldBe` [LoxStmtVoid, LoxStmtVoid, LoxStmtVoid, LoxStmtPrint "4"]

interpretExpr' :: Text -> IO (Either InterpretError LoxValue)
interpretExpr' input = do
  loxEnv <- makeLoxEnv
  let env = envEmpty
      (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parseExpr tokens
  pure $ evalState (interpret result) env

interpretStmt' :: Text -> IO (Either InterpretError [LoxStmtValue])
interpretStmt' input = do
  loxEnv <- makeLoxEnv
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parse tokens
  pure $ evalPure result

module HLox.Interpreter.Test where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
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
        result <- interpretExpr' "1 + 5 < 3 * 3"
        result `shouldBe` Right (LoxBool True)
      it "should evaluate number expression " $ do
        result <- interpretExpr' "42 * 2"
        result `shouldBe` Right (LoxNumber 84)
      it "should concatenate strings" $ do
        result <- interpretExpr' [i|"Hello" + " " + "World"|]
        result `shouldBe` Right (LoxText "Hello World")
      it "should stringify lhs operands" $ do
        result <- interpretExpr' [i|42 + "!"|]
        result `shouldBe` Right (LoxText "42!")
      it "should stringify rhs operands" $ do
        result <- interpretExpr' [i|"Answer: " + 42|]
        result `shouldBe` Right (LoxText "Answer: 42")
      it "should evaluate logical and operators" $ do
        result <- interpretExpr' "true and false"
        result `shouldBe` Right (LoxBool False)
      it "should evaluate logical or operators" $ do
        result <- interpretExpr' "false or true"
        result `shouldBe` Right (LoxBool True)
      it "should preserve values based on truthiness" $ do
        result <- sequenceA <$> traverse interpretExpr' [[i|"strings are true" or true|], [i|false or 42|]]
        result `shouldBe` Right [LoxText "strings are true", LoxNumber 42]

spec_interpreterStmt :: SpecWith ()
spec_interpreterStmt = do
  describe "Interpreter" $ do
    describe "valid statements" $ do
      it "should handle variables" $ do
        result <- interpretStmt' "var a = 1; var b = 2; print a + b;"
        result `shouldBe` Right [LoxStmtVoid, LoxStmtVoid, LoxStmtPrint "3"]
      it "should handle re-assignments" $ do
        result <- interpretStmt' "var a = 1; var b = 2; a = 2; print a + b;"
        result `shouldBe` Right [LoxStmtVoid, LoxStmtVoid, LoxStmtVoid, LoxStmtPrint "4"]
      it "should handle block expressions" $ do
        result <- interpretStmt' "var a = 1; { a = 2; var b = 3; print a + b; } print a;"
        result `shouldBe` Right [LoxStmtVoid, LoxStmtBlock [LoxStmtVoid, LoxStmtVoid, LoxStmtPrint "5"], LoxStmtPrint "2"]
      it "should handle block expression re-assignment of outer variables" $ do
        result <- interpretStmt' "var a = 1; { a = 2; } print a;"
        result `shouldBe` Right [LoxStmtVoid, LoxStmtBlock [LoxStmtVoid], LoxStmtPrint "2"]
      it "should handle if statements" $ do
        result <- interpretStmt' "if (true) { print 1; } else { print 2; }"
        result `shouldBe` Right [LoxStmtBlock [LoxStmtPrint "1"]]
      it "should handle if statements without else" $ do
        result <- interpretStmt' "if (true) { print 1; }"
        result `shouldBe` Right [LoxStmtBlock [LoxStmtPrint "1"]]
      it "should handle while statements" $ do
        let program = "var x = 0; while (x < 5) { print x; x = x + 1; } print x;"
        Right result <- interpretStmt' program
        result
          `shouldBe` [ LoxStmtVoid, -- var
                       LoxStmtBlock -- while
                       -- print + assignment
                         [ LoxStmtBlock [LoxStmtPrint "0", LoxStmtVoid],
                           LoxStmtBlock [LoxStmtPrint "1", LoxStmtVoid],
                           LoxStmtBlock [LoxStmtPrint "2", LoxStmtVoid],
                           LoxStmtBlock [LoxStmtPrint "3", LoxStmtVoid],
                           LoxStmtBlock [LoxStmtPrint "4", LoxStmtVoid]
                         ],
                       LoxStmtPrint "5" -- final print
                     ]

interpretExpr' :: Text -> IO (Either InterpretError LoxValue)
interpretExpr' input = do
  loxEnv <- makeLoxEnv
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parseExpr tokens
  runExceptT $ evalStateT (interpret result) initialEnv

interpretStmt' :: Text -> IO (Either InterpretError [LoxStmtValue])
interpretStmt' input = do
  loxEnv <- makeLoxEnv
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parse tokens
  runExceptT (evalPure initialEnv result)

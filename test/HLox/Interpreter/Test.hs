module HLox.Interpreter.Test where

import           Control.Lens.Lens ((<&>))
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State (evalStateT)
import           Control.Monad.Writer (execWriterT, runWriterT)
import           Data.String.Interpolate (i)
import           Data.Text (Text)
import           HLox.Interpreter (evalPure, interpret)
import qualified HLox.Interpreter.Environment as Env
import           HLox.Interpreter.Types
import           HLox.Parser (parse, parseExpr)
import           HLox.Scanner (scanTokens)
import           HLox.Scanner.Types
import           HLox.Types
import           Test.Hspec (SpecWith, describe, it, shouldBe)

spec_environment :: SpecWith ()
spec_environment = do
  describe "Environment" $ do
    it "should contain native functions" $ do
      env <- Env.global
      r <- Env.lookup env "clock"
      r `shouldBe` Just (LoxNativeFun LoxClock)
    it "should push an empty environment" $ do
      env <- Env.global >>= Env.pushEmpty
      Env.size env `shouldBe` 2
    it "should pop an environment" $ do
      env <- Env.global >>= Env.pushEmpty <&> Env.pop
      Env.size env `shouldBe` 1
    it "should define a variable" $ do
      env <- Env.global
      let name = "a"
      Env.define env name (LoxNumber 1)
      r <- Env.lookup env name
      r `shouldBe` Just (LoxNumber 1)
    it "should assign a variable" $ do
      env <- Env.global
      let name = "a"
          t = Token LEFT_PAREN (Lexeme "foo") LitNothing (Line 42)
      Env.define env name (LoxNumber 1)
      assignResult <- runExceptT $ Env.assign env t name (LoxNumber 2)
      assignResult `shouldBe` Right ()
      r <- Env.lookup env name
      r `shouldBe` Just (LoxNumber 2)
    it "should assign a shadowed variable" $ do
      env <- Env.global
      let name = "a"
          t = Token LEFT_PAREN (Lexeme "foo") LitNothing (Line 42)
      Env.define env name (LoxNumber 1)
      env' <- Env.pushEmpty env
      Env.define env' name (LoxNumber 1)
      assignResult <- runExceptT $ Env.assign env' t name (LoxNumber 2)
      assignResult `shouldBe` Right ()
      r1 <- Env.lookup env' name
      r1 `shouldBe` Just (LoxNumber 2)
      let env'' = Env.pop env'
      r2 <- Env.lookup env'' name
      r2 `shouldBe` Just (LoxNumber 1)
    it "should lookup an outer variable" $ do
      env <- Env.global
      let name = "outer"
      Env.define env name (LoxNumber 5)
      env' <- Env.pushEmpty env
      r <- Env.lookup env' name
      r `shouldBe` Just (LoxNumber 5)

spec_interpreterExpr :: SpecWith ()
spec_interpreterExpr = do
  describe "Interpreter (Expr)" $ do
    describe "valid expressions" $ do
      it "should evaluate boolean expression" $ do
        result <- interpretExpr' "1 + 5 < 3 * 3"
        result `shouldBe` Right (LoxBool True, [])
      it "should evaluate number expression " $ do
        result <- interpretExpr' "42 * 2"
        result `shouldBe` Right (LoxNumber 84, [])
      it "should concatenate strings" $ do
        result <- interpretExpr' [i|"Hello" + " " + "World"|]
        result `shouldBe` Right (LoxText "Hello World", [])
      it "should stringify lhs operands" $ do
        result <- interpretExpr' [i|42 + "!"|]
        result `shouldBe` Right (LoxText "42!", [])
      it "should stringify rhs operands" $ do
        result <- interpretExpr' [i|"Answer: " + 42|]
        result `shouldBe` Right (LoxText "Answer: 42", [])
      it "should evaluate logical and operators" $ do
        result <- interpretExpr' "true and false"
        result `shouldBe` Right (LoxBool False, [])
      it "should evaluate logical or operators" $ do
        result <- interpretExpr' "false or true"
        result `shouldBe` Right (LoxBool True, [])
      it "should preserve values based on truthiness" $ do
        result <- sequenceA <$> traverse interpretExpr' [[i|"strings are true" or true|], [i|false or 42|]]
        result `shouldBe` Right [(LoxText "strings are true", []), (LoxNumber 42, [])]

spec_interpreterStmt :: SpecWith ()
spec_interpreterStmt = do
  describe "Interpreter (Stmt)" $ do
    describe "valid statements" $ do
      it "should handle variables" $ do
        result <- interpretStmt' "var a = 1; var b = 2; print a + b;"
        result `shouldBe` Right [LoxEffectPrint "3"]
      it "should handle re-assignments" $ do
        result <- interpretStmt' "var a = 1; var b = 2; a = 2; print a + b;"
        result `shouldBe` Right [LoxEffectPrint "4"]
      it "should handle block expressions" $ do
        result <- interpretStmt' "var a = 1; { a = 2; var b = 3; print a + b; } print a;"
        result `shouldBe` Right [LoxEffectPrint "5", LoxEffectPrint "2"]
      it "should handle block expression re-assignment of outer variables" $ do
        result <- interpretStmt' "var a = 1; { a = 2; } print a;"
        result `shouldBe` Right [LoxEffectPrint "2"]
      it "should handle if statements" $ do
        result <- interpretStmt' "if (true) { print 1; } else { print 2; }"
        result `shouldBe` Right [LoxEffectPrint "1"]
      it "should handle if statements without else" $ do
        result <- interpretStmt' "if (true) { print 1; }"
        result `shouldBe` Right [LoxEffectPrint "1"]
      it "should handle while statements" $ do
        let program = "var x = 0; while (x < 5) { print x; x = x + 1; } print x;"
        result <- interpretStmt' program
        result
          `shouldBe` Right
            [ LoxEffectPrint "0",
              LoxEffectPrint "1",
              LoxEffectPrint "2",
              LoxEffectPrint "3",
              LoxEffectPrint "4",
              LoxEffectPrint "5"
            ]
      it "should handle functions" $ do
        let program =
              [i|fun sayHi(first, last) {
                   print "Hi, " + first + " " + last + "!";
                 }
                 sayHi("Dear", "Reader");
              |]
        result <- interpretStmt' program
        result `shouldBe` Right [LoxEffectPrint "Hi, Dear Reader!"]


spec_interpreterPrograms :: SpecWith ()
spec_interpreterPrograms = do
  describe "Interpreter Programs" $ do
    it "should handle fibonacci" $ do
      let program =
            [i|fun fib(n) {
                 if (n <= 1) return n;
                 return fib(n - 2) + fib(n - 1);
               }
               for (var i = 0; i < 20; i = i + 1) {
                 print fib(i);
               }
            |]
      result <- interpretStmt' program
      result
        `shouldBe` Right
          [ LoxEffectPrint "0",
            LoxEffectPrint "1",
            LoxEffectPrint "1",
            LoxEffectPrint "2",
            LoxEffectPrint "3",
            LoxEffectPrint "5",
            LoxEffectPrint "8",
            LoxEffectPrint "13",
            LoxEffectPrint "21",
            LoxEffectPrint "34",
            LoxEffectPrint "55",
            LoxEffectPrint "89",
            LoxEffectPrint "144",
            LoxEffectPrint "233",
            LoxEffectPrint "377",
            LoxEffectPrint "610",
            LoxEffectPrint "987",
            LoxEffectPrint "1597",
            LoxEffectPrint "2584",
            LoxEffectPrint "4181"
          ]

    it "should handle closures (makeCounter)" $ do
      -- This one forced mutable environments for variables
      let program =
            [i|fun makeCounter() {
                 var i = 0;

                 fun count() {
                   i = i + 1;
                   print i;
                 }

                 return count;
               }

               var counter = makeCounter();
               counter();
               counter();
            |]
      result <- interpretStmt' program
      result
        `shouldBe` Right
          [ LoxEffectPrint "1",
            LoxEffectPrint "2"
          ]

    it "have lexical scoping" $ do
      let program =
            [i|var a = "global";
               {
                 fun showA() {
                   print a;
                 }

                 showA();
                 var a = "block";
                 showA();
               }
            |]
      result <- interpretStmt' program
      result
        `shouldBe` Right
          [ LoxEffectPrint "global",
            LoxEffectPrint "global"
          ]

interpretExpr' :: Text -> IO (Either InterpretError (LoxValue, [LoxEffect]))
interpretExpr' input = do
  loxEnv <- makeLoxEnv
  env <- Env.global
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parseExpr tokens
  runExceptT $ runWriterT $ evalStateT (interpret result) env

interpretStmt' :: Text -> IO (Either InterpretError [LoxEffect])
interpretStmt' input = do
  loxEnv <- makeLoxEnv
  env <- Env.global
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parse tokens
  runExceptT $ execWriterT $ flip evalStateT env $ evalPure result

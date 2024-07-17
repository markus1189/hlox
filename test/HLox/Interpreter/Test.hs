module HLox.Interpreter.Test where

import Control.Monad.Reader (runReader)
import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Interpreter (interpret)
import HLox.Interpreter.Types
import HLox.Parser (parseExpr)
import HLox.Scanner (scanTokens)
import HLox.Types
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec_interpreter :: SpecWith ()
spec_interpreter = do
  describe "Interpreter" $ do
    describe "valid expressions" $ do
      it "should evaluate boolean expression" $ do
        Right result <- interpret' "1 + 5 < 3 * 3"
        result `shouldBe` LoxBool True
      it "should evaluate number expression " $ do
        Right result <- interpret' "42 * 2"
        result `shouldBe` LoxNumber 84
      it "should concatenate strings" $ do
        Right result <- interpret' [i|"Hello" + " " + "World"|]
        result `shouldBe` LoxText "Hello World"
      it "should stringify lhs operands" $ do
        Right result <- interpret' [i|42 + "!"|]
        result `shouldBe` LoxText "42!"
      it "should stringify rhs operands" $ do
        Right result <- interpret' [i|"Answer: " + 42|]
        result `shouldBe` LoxText "Answer: 42"

interpret' :: Text -> IO (Either InterpretError LoxValue)
interpret' input = do
  loxEnv <- makeLoxEnv
  let env = envEmpty
      (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ parseExpr tokens
  pure $ runReader (interpret result) env

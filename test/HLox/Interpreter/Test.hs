module HLox.Interpreter.Test where

import HLox.Interpreter.Types (LoxValue(..))
import Test.Hspec (shouldBe, describe, it, SpecWith)
import HLox.Types
import HLox.Scanner (scanTokens)
import HLox.Parser (parse)
import HLox.Interpreter (interpret)
import Data.Text (Text)
import Data.String.Interpolate (i)

spec_interpreter :: SpecWith ()
spec_interpreter = do
  describe "Interpreter" $ do
    describe "valid expressions" $ do
      it "should evaluate boolean expression" $ do
        result <- interpret' "1 + 5 < 3 * 3"
        result `shouldBe` LoxBool True
      it "should evaluate number expression " $ do
        result <- interpret' "42 * 2"
        result `shouldBe` LoxNumber 84
      it "should concatenate strings" $ do
        result <- interpret' [i|"Hello" + " " + "World"|]
        result `shouldBe` LoxText "Hello World"

interpret' :: Text -> IO LoxValue
interpret' input = do
  env <- makeLoxEnv
  let (tokens, _) = scanTokens input
  Right result <- flip runLox env $ parse tokens
  pure $ interpret result

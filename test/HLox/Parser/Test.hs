module HLox.Parser.Test where

import Data.String.Conversions (convertString)
import Data.Text (Text)
import HLox.Parser (parse, pretty)
import HLox.Parser.Types
import HLox.Scanner (scanTokens)
import HLox.Scanner.Types
import HLox.Types (makeLoxEnv, runLox)
import System.FilePath ((</>))
import Test.Hspec (SpecWith, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

testValidParser :: String -> Text -> TestTree
testValidParser name input = goldenVsString name ("golden" </> "parser" </> name) $ do
  let (tokens, _) = scanTokens input
  env <- makeLoxEnv
  Right result <- flip runLox env $ parse tokens
  pure $ convertString $ pretty result

spec_simpleAst :: SpecWith ()
spec_simpleAst = do
  describe "Pretty printer" $ do
    it "should print a simple ast" $ do
      let e =
            ExprBinary
              (ExprUnary (Token MINUS (Lexeme "-") LitNothing (Line 1)) (ExprLiteral (LitNumber 123)))
              (Token STAR (Lexeme "*") LitNothing (Line 1))
              (ExprGrouping (ExprLiteral (LitNumber 45.67)))
          result = pretty e
      result `shouldBe` "(* (- 123) (group 45.67))"

  describe "Parser" $ do
    it "should parse a number" $ do
      env <- makeLoxEnv
      result <-
        flip runLox env $
          parse
            [ Token NUMBER (Lexeme "42") (LitNumber 42) (Line 1),
              Token EOF (Lexeme "") LitNothing (Line 1)
            ]
      result `shouldBe` Right (ExprLiteral (LitNumber 42))

    it "should parse an expression" $ do
      let (tokens, _) = scanTokens "1 * 2 + 3 == 6"
      env <- makeLoxEnv
      result <-
        flip runLox env $
          parse tokens
      case result of
        Left _ -> expectationFailure "Could not scan"
        Right expr -> pretty expr `shouldBe` "(== (+ (* 1 2) 3) 6)"

test_goldenParser :: TestTree
test_goldenParser =
  testGroup
    "Golden Tests"
    [ testValidParser "arithmetic_equality" "1 * 2 + 3 == 6",
      testValidParser "multi_equality" "1 == 2 == 3 == 4"
    ]

module HLox.Parser.Test where

import Control.Exception (throwIO)
import Data.String.Conversions (convertString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Parser (parse, parseExpr, pretty)
import HLox.Parser.Types
import HLox.Scanner (scanTokens)
import HLox.Scanner.Types
import HLox.Types (makeLoxEnv, runLox)
import System.FilePath ((</>))
import Test.Hspec (SpecWith, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

testValidParserExpr :: String -> Text -> TestTree
testValidParserExpr name input = goldenVsString name ("golden" </> "parser" </> name) $ do
  let (tokens, _) = scanTokens input
  env <- makeLoxEnv
  result <- flip runLox env $ parseExpr tokens
  case result of
    Left err -> throwIO err
    Right expr -> pure $ convertString $ pretty expr

testValidParser :: String -> Text -> TestTree
testValidParser name input = goldenVsString name ("golden" </> "parser_stmt" </> name) $ do
  let (tokens, _) = scanTokens input
  env <- makeLoxEnv
  result <- flip runLox env $ parse tokens
  case result of
    Left err -> throwIO err
    Right expr -> pure $ convertString $ pretty expr

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
    describe "expressions" $ do
      it "should parse a number" $ do
        env <- makeLoxEnv
        result <-
          flip runLox env $
            parseExpr
              [ Token NUMBER (Lexeme "42") (LitNumber 42) (Line 1),
                Token EOF (Lexeme "") LitNothing (Line 1)
              ]
        result `shouldBe` Right (ExprLiteral (LitNumber 42))

      it "should parse an expression" $ do
        let (tokens, _) = scanTokens "1 * 2 + 3 == 6"
        env <- makeLoxEnv
        result <-
          flip runLox env $
            parseExpr tokens
        case result of
          Left _ -> expectationFailure "Could not scan"
          Right expr -> pretty expr `shouldBe` "(== (+ (* 1 2) 3) 6)"
    describe "statements" $ do
      it "should parse a statement" $ do
        let (tokens, _) = scanTokens "print 42;"
        env <- makeLoxEnv
        result <-
          flip runLox env $
            parse tokens
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (print 42))"

test_goldenParser :: TestTree
test_goldenParser =
  testGroup
    "Golden Tests"
    [ testValidParserExpr "arithmetic_equality" "1 * 2 + 3 == 6",
      testValidParserExpr "multi_equality" "1 == 2 == 3 == 4",
      testValidParser "statements" "1 == 4; 1 > 3;"
    ]

module HLox.Parser.Test where

import Control.Exception (throwIO)
import Control.Lens.Operators ((.~), (<&>))
import Data.String.Conversions (convertString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.UUID qualified as UUID
import HLox.Parser (parse, parseExpr, pretty)
import HLox.Parser.Types
import HLox.Scanner (scanTokens)
import HLox.Scanner.Types
import HLox.Types (Lox, makeLoxEnv, runLox)
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
    Right expr -> pure . convertString $ pretty expr

testValidParser :: String -> Text -> TestTree
testValidParser name input = goldenVsString name ("golden" </> "parser_stmt" </> name) $ do
  let (tokens, _) = scanTokens input
  env <- makeLoxEnv
  result <- flip runLox env $ parse tokens
  case result of
    Left err -> throwIO err
    Right expr -> pure . convertString $ pretty expr

spec_simpleAst :: SpecWith ()
spec_simpleAst = do
  describe "Pretty printer" . it "should print a simple ast" $
    ( do
        let e =
              ExprBinary
                UUID.nil
                (ExprUnary UUID.nil (Token MINUS (Lexeme "-") LitNothing (Line 1)) (ExprLiteral UUID.nil (LitNumber 123)))
                (Token STAR (Lexeme "*") LitNothing (Line 1))
                (ExprGrouping UUID.nil (ExprLiteral UUID.nil (LitNumber 45.67)))
            result = pretty e
        result `shouldBe` "(* (- 123) (group 45.67))"
    )

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
        (result <&> exprId .~ UUID.nil) `shouldBe` Right (ExprLiteral UUID.nil (LitNumber 42))

      it "should parse an expression" $ do
        result <- parseExpr' "1 * 2 + 3 == 6"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(== (+ (* 1 2) 3) 6)"

      it "should parse 'or' and 'and'" $ do
        result <- fmap pretty <$> parseExpr' "true and true and true or false or true and false"
        result `shouldBe` Right "(or (or (and (and True True) True) False) (and True False))"

    describe "statements" $ do
      it "should parse a statement" $ do
        result <- parse' "print 42;"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (print 42))"
      it "should parse variables" $ do
        result <- parse' "var a = 1; var b = 2; print a + b;"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (assign a 1) (assign b 2) (print (+ a b)))"
      it "should parse blocks" $ do
        result <- parse' "{ var a = 1; print a; }"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (block (assign a 1) (print a)))"
      it "should parse blocks (2)" $ do
        result <- parse' "var outer = true; { var a = 1; print a; } print outer;"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (assign outer True) (block (assign a 1) (print a)) (print outer))"
      it "should parse empty blocks" $ do
        result <- parse' "{} print 1;"
        case result of
          Left err -> expectationFailure [i|Could not parse the statement: #{err}|]
          Right expr -> pretty expr `shouldBe` "(sequence (block ) (print 1))"

      it "should parse if statements" $ do
        result <- fmap pretty <$> parse' "if (1 > 0) print \"true\"; else print \"false\";"
        result `shouldBe` Right "(sequence (if (> 1 0) (print true) (print false)))"

      it "should parse while statements" $ do
        result <- fmap pretty <$> parse' "while (1 and 2) { print 5; }"
        result `shouldBe` Right "(sequence (while (and 1 2) (block (print 5))))"

      it "should parse for loops" $ do
        result <- fmap pretty <$> parse' "for (var a = 1; a < 10; a = a + 1) { print a; }"
        result `shouldBe` Right "(sequence (block (assign a 1) (while (< a 10) (block (block (print a)) (reassign a (+ a 1))))))"

      it "should parse functions" $ do
        result <- fmap pretty <$> parse' "fun f() { print \"42\"; }"
        result `shouldBe` Right "(sequence (declare-fun f () (sequence (print 42))))"

      it "should parse function calls" $ do
        result <- fmap pretty <$> parse' "average(1, 2);"
        result `shouldBe` Right "(sequence (call average (list 1 2)))"

      it "should parse classes" $ do
        result <-
          fmap pretty
            <$> parse'
              [i|class DevonshireCream {
                                              serveOn() {
                                                return "Scones";
                                              }
                                            }

                                            print DevonshireCream;
                                           |]
        result `shouldBe` Right "(sequence (class DevonshireCream (methods (declare-fun serveOn () (sequence (return Scones))))) (print DevonshireCream))"

test_goldenParser :: TestTree
test_goldenParser =
  testGroup
    "Golden Tests"
    [ testValidParserExpr "arithmetic_equality" "1 * 2 + 3 == 6",
      testValidParserExpr "multi_equality" "1 == 2 == 3 == 4",
      testValidParser "statements" "1 == 4; 1 > 3;"
    ]

parseExpr' :: Text -> IO (Either ParseError Expr)
parseExpr' = parseWith parseExpr

parse' :: Text -> IO (Either ParseError [Stmt])
parse' = parseWith parse

parseWith :: ([Token] -> Lox b) -> Text -> IO b
parseWith p input = do
  let (tokens, []) = scanTokens input
  env <- makeLoxEnv
  flip runLox env $ p tokens

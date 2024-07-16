module HLox.Parser.Test where

import HLox.Parser (parse, pretty)
import HLox.Parser.Types
import HLox.Scanner.Types
import HLox.Types (makeLoxEnv, runLox)
import Test.Hspec (SpecWith, describe, it, shouldBe)

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
      result <- flip runLox env $ parse [Token NUMBER (Lexeme "42") (LitNumber 42) (Line 1), Token EOF (Lexeme "") LitNothing (Line 1)]
      result `shouldBe` (Right (ExprLiteral (LitNumber 42)))

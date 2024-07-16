module HLox.Parser.Test where

import HLox.Parser.Types
import HLox.Scanner.Types
import Test.Hspec (shouldBe, describe, it, SpecWith)
import HLox.Parser (pretty)

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

module HLox.Resolver.Test where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import HLox.Parser (parse)
import HLox.Resolver (resolve)
import HLox.Resolver.Types
import HLox.Scanner (scanTokens)
import HLox.Scanner.Types
import HLox.Types
import HLox.Util (loxHadError)
import Test.Hspec

spec_simpleAst :: SpecWith ()
spec_simpleAst = do
  describe "Resolver" $ do
    it "should detect top level returns" $ do
      result <- resolve' [i|return "at top level";|]
      result `shouldBe` (DepthMap mempty, [ResolverError (Token RETURN (Lexeme "return") LitNothing (Line 1)) "Can't return from top-level code."])
    it "should detect invalid initializer usage" $ do
      result <- resolve' [i|{ var a = "outer"; { var a = a; } }|]
      result `shouldBe` (DepthMap mempty, [ResolverError (Token IDENTIFIER (Lexeme "a") LitNothing (Line 1)) "Cannot read local variable in its own initializer"])
    it "should detect multi declaration in scopes" $ do
      result <- resolve' [i|{ var a = "first"; var a = "second"; }|]
      result `shouldBe` (DepthMap mempty, [ResolverError (Token IDENTIFIER (Lexeme "a") LitNothing (Line 1)) "Already a variable with this name in this scope."])
    it "should detect usage of 'this' outside a class" $ do
      result <- resolve' [i|{ print this; }|]
      result `shouldBe` (DepthMap mempty, [ResolverError (Token THIS (Lexeme "this") LitNothing (Line 1)) "Can't use 'this' outside of a class."])

resolve' :: Text -> IO (DepthMap, [ResolverError])
resolve' input = do
  loxEnv <- makeLoxEnv
  let (tokens, _) = scanTokens input
  Right result <- flip runLox loxEnv $ do
    r <- parse tokens
    whenM loxHadError . liftIO $ expectationFailure "Errors while parsing"
    pure r

  pure $ resolve result

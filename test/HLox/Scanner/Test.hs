module HLox.Scanner.Test where

import Control.Lens.Operators ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.TH (Options (constructorTagModifier, fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import HLox.Scanner (Literal (..), ScanError (..), Token (..), scanTokens, _Lexeme, _Line)
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

data StoredLiteral = StoredLitNothing | StoredLitText !Text | StoredLitNumber !Double | StoredLitBool !Bool

$(deriveJSON defaultOptions {constructorTagModifier = drop (length @[] "StoredLit")} ''StoredLiteral)

data StoredToken = StoredToken
  { _storedTokenType :: !String,
    _storedTokenLexeme :: !Text,
    _storedTokenLiteral :: !StoredLiteral,
    _storedTokenLine :: !Natural
  }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (length @[] "_storedToken")} ''StoredToken)

data StoredScanError = StoredScanError !Natural !Text

$(deriveJSON defaultOptions {constructorTagModifier = drop (length @[] "StoredScan")} ''StoredScanError)

testScanner :: String -> Text -> TestTree
testScanner name input = goldenVsString name ("golden" </> name) $ do
  let (tokens, errors) = scanTokens input
  pure . encodePretty $
    Aeson.object
      [ "tokens" Aeson..= fmap toStoredToken tokens,
        "errors" Aeson..= fmap toStoredScanError errors
      ]

toStoredToken :: Token -> StoredToken
toStoredToken (Token t l lit line) = StoredToken (show t) (l ^. _Lexeme) (toStoredLiteral lit) (line ^. _Line)

toStoredLiteral :: Literal -> StoredLiteral
toStoredLiteral (LitText t) = StoredLitText t
toStoredLiteral (LitNumber n) = StoredLitNumber n
toStoredLiteral LitNothing = StoredLitNothing
toStoredLiteral (LitBool b) = StoredLitBool b

toStoredScanError :: ScanError -> StoredScanError
toStoredScanError (ScanError l t) = StoredScanError (l ^. _Line) t

test_goldenScanner :: TestTree
test_goldenScanner =
  testGroup
    "Golden Tests"
    [ testScanner "numbers" "1 1.2 .1 1.",
      testScanner "operators" ">= <= != == > < / + -",
      testScanner "strings" "\"hello\" \"world\"",
      testScanner "keywords" "and class else false for fun if nil or print return super this true var while",
      testScanner "comments" $
        Text.intercalate
          "\n"
          [ "// this is a comment",
            "// this is another comment",
            "/* this is a \nblock comment */",
            "/* this comment is not closed\n1 + 1 == 5",
            "/* stars inside: 3*3 5 * 1 */"
          ],
      testScanner "booleans" "true false"
    ]

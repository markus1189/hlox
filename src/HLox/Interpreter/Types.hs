module HLox.Interpreter.Types where

import Control.Lens.Combinators (at, over, view)
import Control.Lens.TH (makePrisms)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Scanner.Types

data LoxStmtValue
  = LoxStmtVoid
  | LoxStmtPrint !Text
  deriving (Show)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  deriving (Show, Eq, Ord)

makePrisms ''LoxValue

stringify :: LoxValue -> Text
stringify LoxNil = "nil"
stringify (LoxNumber n) = sformat shortest n
stringify (LoxText t) = t
stringify (LoxBool True) = "true"
stringify (LoxBool False) = "false"

data InterpretError = InterpretError !Token !Text
  deriving (Show)

newtype Environment = Environment (Map Text LoxValue) deriving (Show)

makePrisms ''Environment

envDefine :: Text -> LoxValue -> Environment -> Environment
envDefine name value = over _Environment $ Map.insert name value

envGet :: Text -> Environment -> Maybe LoxValue
envGet name = view (_Environment . at name)

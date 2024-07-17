module HLox.Interpreter.Types where

import Control.Lens.TH (makePrisms)
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Scanner.Types

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

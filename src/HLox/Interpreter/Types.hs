module HLox.Interpreter.Types where

import Control.Lens.TH (makePrisms)
import Data.Text (Text)
import HLox.Scanner.Types

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  deriving (Show, Eq, Ord)

makePrisms ''LoxValue

data InterpretError = InterpretError !Token !Text
  deriving (Show)

module HLox.Interpreter.Types where

import Data.Text (Text)
import Control.Lens.TH (makePrisms)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  deriving (Show, Eq, Ord)

makePrisms ''LoxValue

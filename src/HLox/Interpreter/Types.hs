module HLox.Interpreter.Types where

import Control.Lens (At, Index, IxValue, Lens', Traversal', ix)
import Control.Lens.Combinators (Ixed, at)
import Control.Lens.TH (makePrisms)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types

data LoxStmtValue
  = LoxStmtVoid
  | LoxStmtPrint !Text
  | LoxStmtBlock ![LoxStmtValue]
  deriving (Show, Eq, Ord)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  deriving (Show, Eq, Ord)

makePrisms ''LoxValue

instance Pretty LoxValue where
  pretty = stringify

stringify :: LoxValue -> Text
stringify LoxNil = "nil"
stringify (LoxNumber n) = sformat shortest n
stringify (LoxText t) = t
stringify (LoxBool True) = "true"
stringify (LoxBool False) = "false"

data InterpretError = InterpretError !Token !Text
  deriving (Show)

newtype Environment = Environment (Map Text LoxValue) deriving (Show, Eq, Ord, Semigroup, Monoid)

makePrisms ''Environment

type instance IxValue Environment = LoxValue

type instance Index Environment = Text

instance Ixed Environment where
  ix :: Index Environment -> Traversal' Environment (IxValue Environment)
  ix name = _Environment . ix name

instance At Environment where
  at :: Index Environment -> Lens' Environment (Maybe (IxValue Environment))
  at name = _Environment . at name

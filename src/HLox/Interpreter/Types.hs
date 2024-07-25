module HLox.Interpreter.Types where

import Control.Lens.TH (makeFields, makePrisms)
import Data.HashTable.IO qualified as H
import Data.List.NonEmpty (NonEmpty)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Parser.Types (Stmt)
import HLox.Pretty (Pretty, pretty)
import HLox.Resolver.Types (DepthMap, HasDepthMap(..))
import HLox.Scanner.Types (Token)

type HashTable k v = H.CuckooHashTable k v

data LoxNativeFunKind = LoxClock deriving (Show, Eq, Ord)

newtype LoxEffect = LoxEffectPrint Text deriving (Show, Eq, Ord)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  | LoxFun ![Text] !Environment ![Stmt]
  | LoxNativeFun !LoxNativeFunKind
  | LoxClass !Klass
  | LoxInstance !Klass InstanceFields
  deriving (Show, Eq, Ord)

data Klass = Klass !Text deriving (Show, Eq, Ord)

newtype InstanceFields = InstanceFields (HashTable Text LoxValue)  deriving (Show)

instance Eq InstanceFields where
  _ == _ = False

instance Ord InstanceFields where
  compare _ _ = EQ

instance Pretty LoxValue where
  pretty = stringify

stringify :: LoxValue -> Text
stringify LoxNil = "nil"
stringify (LoxNumber n) = sformat shortest n
stringify (LoxText t) = t
stringify (LoxBool True) = "true"
stringify (LoxBool False) = "false"
stringify (LoxFun {}) = "<function>"
stringify (LoxNativeFun k) = [i|<native function: #{show k}>|]
stringify (LoxClass (Klass n)) = [i|<class #{n}>|]
stringify (LoxInstance (Klass n) (InstanceFields fields)) = [i|<instance #{n} #{fields}>|]

data InterpretError
  = InterpretRuntimeError !Token !Text
  | InterpretReturn !LoxValue
  deriving (Show, Eq)

newtype Environment = Environment (NonEmpty (HashTable Text LoxValue)) deriving (Show)

instance Eq Environment where
  (==) _ _ = False

instance Ord Environment where
  compare _ _ = EQ

makePrisms ''LoxValue
makePrisms ''Environment

data InterpreterState = InterpreterState
  { interpreterStateEnvironment :: Environment,
    interpreterStateDepthMap :: DepthMap
  }

makeFields ''InterpreterState
makePrisms ''InstanceFields

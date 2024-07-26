module HLox.Resolver.Types where

import Control.Lens.TH (makeFields, makePrisms)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import HLox.Scanner.Types

data InitializeStatus = Uninitialized | Initialized deriving (Show, Eq, Ord)

newtype ScopeStack = ScopeStack [Map Text InitializeStatus] deriving (Show, Eq, Ord)

newtype DepthMap = DepthMap (Map UUID Int) deriving (Show, Eq, Ord)

data ResolverError = ResolverError !Token !Text deriving (Show, Eq)

data FunctionType
  = FunctionTypeNone
  | FunctionTypeFunction
  | FunctionTypeMethod
  | FunctionTypeIntializer
  deriving (Show, Eq, Ord)

data ClassType
  = ClassTypeNone
  | ClassTypeClass
  | ClassTypeSubclass
  deriving (Show, Eq, Ord)

data ResolverState = ResolverState
  { resolverStateScopeStack :: !ScopeStack,
    resolverStateDepthMap :: !DepthMap,
    resolverStateCurrentFunction :: !FunctionType,
    resolverStateClassType :: !ClassType
  }

makeFields ''ResolverState
makePrisms ''ScopeStack
makePrisms ''DepthMap

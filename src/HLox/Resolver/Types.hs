module HLox.Resolver.Types where

import Control.Lens.TH (makePrisms)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import HLox.Scanner.Types

data InitializeStatus = Uninitialized | Initialized deriving (Show, Eq, Ord)

newtype ScopeStack = ScopeStack [Map Text InitializeStatus] deriving (Show, Eq, Ord)

newtype DepthMap = DepthMap (Map UUID Int) deriving (Show, Eq, Ord)

data ResolverError = ResolverError !Token !Text deriving (Show, Eq)

makePrisms ''ScopeStack
makePrisms ''DepthMap

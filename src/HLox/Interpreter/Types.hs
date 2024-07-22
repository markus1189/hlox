module HLox.Interpreter.Types where

import Control.Lens (At, Index, IxValue, Lens', Traversal', ix, _head)
import Control.Lens.Combinators (Ixed, at, lens, view)
import Control.Lens.Operators ((%~), (&), (.~), (?~))
import Control.Lens.TH (makePrisms)
import Control.Monad (join)
import Data.Foldable (find)
import Data.List (findIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Formatting (sformat, shortest)
import HLox.Parser.Types (Stmt)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types

data LoxNativeFunKind = LoxClock deriving (Show, Eq, Ord)

data LoxEffect = LoxEffectPrint !Text
  deriving (Show, Eq, Ord)

data LoxValue
  = LoxNil
  | LoxText !Text
  | LoxNumber !Double
  | LoxBool !Bool
  | LoxFun ![Text] ![Stmt]
  | LoxNativeFun !LoxNativeFunKind
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
stringify (LoxFun _ _) = "<function>"
stringify (LoxNativeFun k) = [i|<native function: #{show k}>|]

data InterpretError
  = InterpretRuntimeError !Token !Text
  | InterpretReturn !LoxValue
  deriving (Show, Eq)

newtype Environment = Environment [Map Text LoxValue]

makePrisms ''Environment

initialEnv :: Environment
initialEnv = Environment [Map.fromList [("clock", LoxNativeFun LoxClock)]]

pushEnv :: Environment -> Environment
pushEnv (Environment es) = Environment (mempty : es)

popEnv :: Environment -> Environment
popEnv e@(Environment []) = e
popEnv (Environment (_ : es)) = Environment es

type instance IxValue Environment = LoxValue

type instance Index Environment = Text

instance Ixed Environment where
  ix :: Index Environment -> Traversal' Environment (IxValue Environment)
  ix name = _Environment . traverse . ix name

instance At Environment where
  at :: Text -> Lens' Environment (Maybe LoxValue)
  at name = lens get set
    where
      get :: Environment -> Maybe LoxValue
      get (Environment es) = join $ find isJust $ map (view (at name)) es

      set :: Environment -> Maybe LoxValue -> Environment
      set (Environment es) (Just v) = case findIndex (Map.member name) es of
        Nothing -> Environment $ es & _head %~ (at name ?~ v)
        Just x -> Environment $ es & ix x %~ at name ?~ v
      set env@(Environment es) Nothing = case findIndex (Map.member name) es of
        Nothing -> env
        Just x -> Environment $ es & ix x %~ at name .~ Nothing

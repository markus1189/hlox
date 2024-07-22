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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Formatting (sformat, shortest)
import HLox.Pretty (Pretty, pretty)
import HLox.Scanner.Types
import Numeric.Natural (Natural)

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
  | LoxFun !Text !Natural !([LoxValue] -> IO LoxValue)

makePrisms ''LoxValue

instance Show LoxValue where
  show = show . pretty

instance Eq LoxValue where
  LoxNil == LoxNil = True
  LoxText t1 == LoxText t2 = t1 == t2
  LoxNumber n1 == LoxNumber n2 = n1 == n2
  LoxBool b1 == LoxBool b2 = b1 == b2
  LoxFun _ _ _ == LoxFun _ _ _ = False
  _ == _ = False

instance Pretty LoxValue where
  pretty = stringify

stringify :: LoxValue -> Text
stringify LoxNil = "nil"
stringify (LoxNumber n) = sformat shortest n
stringify (LoxText t) = t
stringify (LoxBool True) = "true"
stringify (LoxBool False) = "false"
stringify (LoxFun arity name _) = [i|<fn #{arity} "#{name}">|]

data InterpretError = InterpretError !Token !Text
  deriving (Show, Eq)

newtype Environment = Environment [Map Text LoxValue]

makePrisms ''Environment

initialEnv :: Environment
initialEnv =
  Environment
    [ Map.fromList
        [("clock", clockFun)]
    ]
  where
    clockFun = LoxFun "clock" 0 $ \_ -> do
      time <- realToFrac <$> getPOSIXTime
      pure $ LoxNumber time

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

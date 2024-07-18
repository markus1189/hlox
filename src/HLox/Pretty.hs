module HLox.Pretty (Pretty(pretty)) where

import Data.Text (Text)

class Pretty a where
  pretty :: a -> Text

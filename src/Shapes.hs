module Shapes where

import Linear.V2
import Data.Strict.List.NonEmpty
import Text.Megaparsec

data Path time space = Path {trail :: NonEmpty a, water :: time}

instance Functor (Path time) where

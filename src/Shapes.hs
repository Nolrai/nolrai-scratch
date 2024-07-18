module Shapes where

import Linear.V2
import Data.Strict.List.NonEmpty
import Text.Megaparsec
import Numeric.Matrix

data Path time space = Path {trail :: NonEmpty (V2 space) , water :: time}

instance Functor (Path time) where
  fmap f Path{..} = Path {trail = f <$> trail, water = water}

type Triangle = (V2 Double, V2 Double, V2 Double)

triangleToMatrix :: Triangle -> Matrix
triangleToMatrix (a, b, c) = fromList 
  [[f v | v <- [a, b, c]] | f <- [(^. _x), (^. _y), const 1]]

toAffine' :: Triangle Double -> Triangle Double -> Matrix Double
toAffine' dom cod = triangleToMatrix cod * inv (triangleToMatrix dom)

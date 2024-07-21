{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Shapes where

import Data.Aeson
import Data.List.NonEmpty hiding (transpose)
import GHC.Generics (Generic)
import Linear.V2
import Linear.V3

data Path time space = Path {trail :: NonEmpty (V2 space), water :: time}
  deriving (Generic, Functor)

instance (ToJSON a) => ToJSON (V2 a)

instance (FromJSON a) => FromJSON (V2 a)

instance (ToJSON a) => ToJSON (V3 a)

instance (FromJSON a) => FromJSON (V3 a)

instance (ToJSON a, ToJSON b) => ToJSON (Path a b) where
  toEncoding :: Path a b -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON a, FromJSON b) => FromJSON (Path a b)

type Triangle = V3 (V2 Double)

data ShapesFile = ShapesFile
  { drawSpaceTri :: Triangle,
    robotSpaceTri :: Triangle,
    paths :: [Path Double Double]
  }
  deriving (Generic)

instance ToJSON ShapesFile where
  toEncoding :: ShapesFile -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ShapesFile

toNGon :: (Floating f) => Int -> Int -> V2 f
toNGon numSides whichPoint = V2 (sin theta) (cos theta)
  where
    theta = pi * (0.5 + (fromIntegral whichPoint + 0.5) / fromIntegral numSides)

exampleFile :: ShapesFile
exampleFile =
  ShapesFile
    { drawSpaceTri = V3 (V2 0 0) (toNGon 5 0) (toNGon 5 (-1)),
      robotSpaceTri = V3 (V2 (sqrt 10) (pi * pi)) (V2 (exp 1) (log 3)) (V2 (-exp (1 / pi)) (cos 1)),
      paths = [Path {trail = toNGon 5 <$> 0 :| [1 .. 4], water = 10.0}]
    }

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Convert (shapesToFlowCmd) where

import Control.Lens ((^.))
import Data.List (concatMap)
import Data.List.NonEmpty hiding (transpose, zipWith)
import FlowCmd
import Linear.Matrix
import Linear.Metric (distance)
import Linear.V2
import Linear.V3
import Shapes

-- convert a shapes file to a list of GCode commands.
shapesToGCode = fmap toGCode . shapesToFlowCmd

-- Consider a point in R^2 as a point on the Z = 1 plain in R^3
v2ToV3 :: V2 Double -> V3 Double
v2ToV3 (V2 x y) = V3 x y 1

-- turn a 2-simplex into its coresponding square matrix by adding a row of 1's and transposing
triangleToMatrix :: Triangle -> M33 Double
triangleToMatrix t = transpose (v2ToV3 <$> t)

-- solve for the linear transformation coresponding to the affine transform that takes dom to cod
toAffine' :: Triangle -> Triangle -> M33 Double
toAffine' dom cod = triangleToMatrix cod * inv33 (triangleToMatrix dom)

-- Apply the affine transformation that takes dom to cod, to a point in R^2.
-- Importantly this will compute m when partially applied, not repeating the work!
toAffine :: Triangle -> Triangle -> V2 Double -> V2 Double
toAffine dom cod = \p -> (m !* v2ToV3 p) ^. _xy
  where
    m = toAffine' dom cod

-- Convert a ShapesFile to list of FlowCmds.
shapesToFlowCmd :: ShapesFile -> [FlowCmd]
shapesToFlowCmd ShapesFile {..} = convertPaths (toAffine drawSpaceTri robotSpaceTri) paths

-- given a transformation, convert paths in dom space to FlowCmds in cod space
convertPaths :: (V2 Double -> V2 Double) -> [Path Double Double] -> [FlowCmd]
convertPaths transformation = concatMap (convertPath transformation)

-- given a transformation covert a single path into FlowCmds
convertPath :: (V2 Double -> V2 Double) -> Path Double Double -> [FlowCmd]
convertPath transformation Path {trail, water} = header ++ body ++ footer
  where
    robotSpaceTrail :: NonEmpty (V2 Double)
    robotSpaceTrail = transformation <$> trail

    start :: V2 Double
    stops :: [V2 Double]
    (start :| stops) = robotSpaceTrail

    header, body, footer :: [FlowCmd]
    header = [MoveFast start, TurnOnWater]
    footer = [TurnOffWater]
    body =
      case stops of
        [] -> [Dwell water] -- water a dot.
        stops@(_ : _) ->
          -- water a path of lines
          let rate = length / water
              length = sum $ zipWith distance (start : stops) stops
              convertLine :: V2 Double -> FlowCmd
              convertLine target = MoveAtRate {target, rate}
           in convertLine <$> stops

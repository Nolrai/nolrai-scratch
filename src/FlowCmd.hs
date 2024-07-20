{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module FlowCmd where
import Linear.V2
import Text.Printf

data FlowCmd 
  = TurnOnWater
  | TurnOffWater
  | Dwell {dwellTime :: Double}
  | MoveFast {target :: V2 Double}
  | MoveAtRate {target :: V2 Double, rate :: Double}

-- Z is used for water pressure, zmin is off, zmax is on.
zmax, zmin :: Double
zmax = 100
zmin = 0

toGCode :: FlowCmd -> String
toGCode TurnOnWater = printf "G0 Z%d" zmax
toGCode TurnOffWater = printf "G0 Z%d" zmin
toGCode Dwell {dwellTime} = printf "G4 P%f" dwellTime
toGCode MoveFast {target = V2 x y} = printf "G0 X%f Y%f" x y
toGCode MoveAtRate {target = V2 x y, rate} = printf "G1 X%f Y%f F%f" x y rate

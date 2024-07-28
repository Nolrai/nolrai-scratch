{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module ShapesTests (shapesTests) where

import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy as BS (null)
import Data.List as List (all)
import Data.List.NonEmpty as NE (NonEmpty (..))
import Linear (Metric (qd, quadrance), V2 (..), V3 (V3))
import Shapes
  ( Path (..),
    ShapesFile (drawSpaceTri, paths, robotSpaceTri),
    exampleFile,
    toNGon,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck as QC
  ( Property,
    testProperty,
    (==>),
  )

eps :: (Floating a) => a
eps = 10 ** (-5)

class ApproxEq a where
  (~=) :: a -> a -> Bool

instance ApproxEq Double where
  (~=) :: Double -> Double -> Bool
  a ~= b = (a - b) * (a - b) < eps

instance (Floating a, Metric f, Metric g, Applicative f, Applicative g, Ord a, Num a) => ApproxEq (f (g a)) where
  (~=) :: f (g a) -> f (g a) -> Bool
  a ~= b = quadrance (qd <$> a <*> b) < eps

instance (ApproxEq a) => ApproxEq [a] where
  (x : xs) ~= (y : ys) = x ~= y && xs ~= ys
  [] ~= [] = True
  [] ~= (_ : _) = False
  (_ : _) ~= [] = False

instance (ApproxEq a) => ApproxEq (NonEmpty a) where
  (~=) :: NonEmpty a -> NonEmpty a -> Bool
  (x :| xs) ~= (y :| ys) = x ~= y && (xs ~= ys)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

instance ApproxEq ShapesFile where
  a ~= b =
    ((~=) `on` drawSpaceTri) a b
      && ((~=) `on` robotSpaceTri) a b

(~@?=) :: (ApproxEq a, Show a) => a -> a -> Assertion
actual ~@?= expected =
  unless (actual ~= expected) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

shapesTests :: TestTree
shapesTests =
  testGroup
    "Shapes Tests"
    [ exampleFileTests,
      toNGonTests,
      shapesFileJsonTests,
      goldenTests,
      pathJsonTests
    ]

exampleFileTests :: TestTree
exampleFileTests =
  testGroup
    "exampleFile"
    [ testCase "has the correct drawSpaceTri" $ drawSpaceTri exampleFile ~@?= expectedDrawSpaceTri,
      testCase "has the correct robotSpaceTri" $ robotSpaceTri exampleFile ~@?= expectedRobotSpaceTri,
      testCase "has the correct paths" $ paths exampleFile @?= [expectedPath]
    ]
  where
    expectedDrawSpaceTri =
      V3
        (V2 1 0)
        (V2 (-1 / 2) (sqrt 3 / 2))
        (V2 (-1 / 2) (-sqrt 3 / 2))
    expectedRobotSpaceTri =
      V3
        (V2 3.162277 9.869604)
        (V2 2.718281 1.098612)
        (V2 (-0.043213) (-0.540302))
    expectedPath =
      Path
        { trail =
            V2 0.587785 0.809016
              :| [ V2 0.951056 0.309016,
                   V2 0.951056 (-0.309016),
                   V2 0.587785 (-0.809016)
                 ],
          water = 10.0
        }

toNGonTests :: TestTree
toNGonTests =
  testGroup
    "toNGon"
    [ testCase "returns the correct points for a triangle" $ do
        toNGon 3 0 @?= (V2 0.8660254037844386 0.5 :: V2 Double)
        toNGon 3 1 @?= (V2 (-0.8660254037844384) 0.5 :: V2 Double)
        toNGon 3 2 @?= (V2 0 (-1.0) :: V2 Double),
      testCase "returns the correct points for a pentagon" $ do
        toNGon 5 0 @?= (V2 0.5877852522924731 0.8090169943749475 :: V2 Double)
        toNGon 5 1 @?= (V2 0.9510565162951535 0.30901699437494745 :: V2 Double)
        toNGon 5 2 @?= (V2 0.9510565162951536 (-0.30901699437494734) :: V2 Double)
        toNGon 5 3 @?= (V2 0.5877852522924732 (-0.8090169943749473) :: V2 Double)
        toNGon 5 4 @?= (V2 6.123233995736766e-17 (-1.0) :: V2 Double),
      testCase "returns the correct points for a hexagon" $ do
        toNGon 6 0 @?= (V2 0.8660254037844386 0.5 :: V2 Double)
        toNGon 6 1 @?= (V2 0.49999999999999994 0.8660254037844386 :: V2 Double)
        toNGon 6 2 @?= (V2 (-0.16666666666666674) 0.9660254037844386 :: V2 Double)
        toNGon 6 3 @?= (V2 (-0.6666666666666667) 0.5 :: V2 Double)
        toNGon 6 4 @?= (V2 (-0.6666666666666667) (-0.5) :: V2 Double)
        toNGon 6 5 @?= (V2 (-0.1666666666666668) (-0.9660254037844385) :: V2 Double),
      QC.testProperty "toNGon produces points on the unit circle" prop_toNGonUnitCircle
    ]

prop_toNGonUnitCircle :: Int -> Property
prop_toNGonUnitCircle n = n > 2 ==> List.all (\i -> let V2 x y = toNGon n i in abs (sqrt (x * x + y * y) - 1) < 1e-10) [0 .. n - 1]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Shapes Golden Tests"
    [ goldenVsStringDiff
        "Encode the example ShapeFile datastructure."
        (\ref new -> ["diff", "-u", ref, new])
        "golden/exampleFile.shapes"
        (pure $ encode exampleFile)
    ]

shapesFileJsonTests :: TestTree
shapesFileJsonTests =
  testGroup
    "ShapesFile JSON serialization"
    [ testCase "encodes ShapesFile to JSON" $ do
        let json = encode exampleFile
        assertBool "encode should never produce empty json" (not $ BS.null json),
      testCase "decodes ShapesFile from JSON" $ do
        let json = encode exampleFile
        decode json @?= Just exampleFile
    ]

simplePath :: Path Double Double
simplePath = Path {trail = V2 0 0 :| [V2 1 1], water = 10.0}

pathJsonTests :: TestTree
pathJsonTests =
  testGroup
    "Path JSON serialization"
    [ testCase "encodes Path to JSON" $ do
        let json = encode simplePath
        assertBool "encode should never produce empty json" (not $ BS.null json),
      testCase "decodes Path from JSON" $ do
        let json = encode simplePath
        decode json @?= Just simplePath
    ]

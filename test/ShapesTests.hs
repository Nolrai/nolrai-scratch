{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ShapesTests where

import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.Aeson.Types
import Data.ByteString.Lazy as BS (null)
import Data.List as List (all, and)
import Data.List.NonEmpty as NE (NonEmpty (..))
import Data.Typeable (Proxy (..))
import Linear (Metric (dot, qd, quadrance), V2 (..), V3 (V3))
import Shapes
  ( Path (..),
    ShapesFile (..),
    exampleFile,
    toNGon,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Positive (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
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

instance ApproxEq (V2 Double) where
  (~=) :: V2 Double -> V2 Double -> Bool
  a ~= b = qd a b < eps

instance
  {-# OVERLAPPABLE #-}
  (Floating a, Metric g, Applicative g, Ord a, Num a) =>
  ApproxEq (V3 (g a))
  where
  (~=) :: V3 (g a) -> V3 (g a) -> Bool
  a ~= b = quadrance (qd <$> a <*> b) < eps

instance {-# OVERLAPPING #-} (ApproxEq a) => ApproxEq [a] where
  (x : xs) ~= (y : ys) = x ~= y && xs ~= ys
  [] ~= [] = True
  [] ~= (_ : _) = False
  (_ : _) ~= [] = False

instance {-# OVERLAPPING #-} (ApproxEq a) => ApproxEq (NonEmpty a) where
  (~=) :: NonEmpty a -> NonEmpty a -> Bool
  (x :| xs) ~= (y :| ys) = x ~= y && (xs ~= ys)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

instance ApproxEq ShapesFile where
  (~=) :: ShapesFile -> ShapesFile -> Bool
  a ~= b =
    ((~=) `on` drawSpaceTri) a b
      && ((~=) `on` robotSpaceTri) a b

instance (ApproxEq a, ApproxEq (V2 b)) => ApproxEq (Path a b) where
  (~=) :: Path a b -> Path a b -> Bool
  a ~= b =
    ((~=) `on` trail) a b
      && ((~=) `on` water) a b

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
      jsonTests (Proxy :: Proxy ShapesFile),
      jsonTests (Proxy :: Proxy (Path Double Double)),
      goldenTests
    ]

exampleFileTests :: TestTree
exampleFileTests =
  testGroup
    "exampleFile"
    [ testCase "has the correct drawSpaceTri" $ drawSpaceTri exampleFile ~@?= expectedDrawSpaceTri,
      testCase "has the correct robotSpaceTri" $ robotSpaceTri exampleFile ~@?= expectedRobotSpaceTri,
      testCase "has the correct paths" $ paths exampleFile ~@?= [expectedPath]
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
            V2 1.0 0.0
              :| [ V2 0.30901699437494745 0.9510565162951535,
                   V2 (-0.8090169943749473) 0.5877852522924732,
                   V2 (-0.8090169943749476) (-0.587785252292473),
                   V2 0.30901699437494723 (-0.9510565162951536)
                 ],
          water = 10.0
        }

toNGonTests :: TestTree
toNGonTests =
  testGroup
    "toNGon"
    [ testCase "returns the correct points for a triangle" $ do
        toNGon 3 0 ~@?= (V2 1 0 :: V2 Double)
        toNGon 3 1 ~@?= (V2 (-0.5) (sqrt 3 / 2) :: V2 Double)
        toNGon 3 2 ~@?= (V2 (-0.5) (-sqrt 3 / 2) :: V2 Double),
      testCase "returns the correct points for a pentagon" $ do
        toNGon 5 0 ~@?= (V2 1 0 :: V2 Double)
        toNGon 5 1 ~@?= (V2 (cos (2 * pi / 5)) (sin (2 * pi / 5)) :: V2 Double)
        toNGon 5 2 ~@?= (V2 (cos (4 * pi / 5)) (sin (4 * pi / 5)) :: V2 Double)
        toNGon 5 3 ~@?= (V2 (cos (6 * pi / 5)) (sin (6 * pi / 5)) :: V2 Double)
        toNGon 5 4 ~@?= (V2 (cos (8 * pi / 5)) (sin (8 * pi / 5)) :: V2 Double),
      testCase "returns the correct points for a hexagon" $ do
        toNGon 6 0 ~@?= (V2 1 0 :: V2 Double)
        toNGon 6 1 ~@?= (V2 (cos (pi / 3)) (sin (pi / 3)) :: V2 Double)
        toNGon 6 2 ~@?= (V2 (cos (2 * pi / 3)) (sin (2 * pi / 3)) :: V2 Double)
        toNGon 6 3 ~@?= (V2 (cos pi) (sin pi) :: V2 Double)
        toNGon 6 4 ~@?= (V2 (cos (4 * pi / 3)) (sin (4 * pi / 3)) :: V2 Double)
        toNGon 6 5 ~@?= (V2 (cos (5 * pi / 3)) (sin (5 * pi / 3)) :: V2 Double),
      QC.testProperty "toNGon produces points on the unit circle" prop_toNGonUnitCircle,
      QC.testProperty "toNGon points have correct dot products" prop_toNGonAngle,
      QC.testProperty "toNGon points sum to 0" prop_toNGonSum
    ]

prop_toNGonUnitCircle :: Positive Int -> Property
prop_toNGonUnitCircle (Positive n) = n > 2 ==> List.all (\i -> let V2 x y = toNGon n i in (sqrt (x * x + y * y) ~= 1)) [0 .. n - 1]

prop_toNGonAngle :: Positive Int -> Property
prop_toNGonAngle (Positive n) = n > 2 ==>
  List.and $
    do
      (x :: Int) <- [0 .. n - 1]
      (y :: Int) <- [0 .. n - 1]
      let sides = x - y
      let (theta :: Double) = 2 * pi * fromIntegral sides / fromIntegral n
      pure $ (toNGon n x `dot` toNGon n y) ~= cos theta :: [Bool]

prop_toNGonSum :: Positive Int -> Property
prop_toNGonSum (Positive n) = n > 2 ==> sum (toNGon n <$> [0 .. n - 1]) ~= V2 0 0

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

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary :: Gen (NonEmpty a)
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary :: Gen (V3 a)
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Path a b) where
  arbitrary :: Gen (Path a b)
  arbitrary = Path <$> arbitrary <*> arbitrary

instance Arbitrary ShapesFile where
  arbitrary :: Gen ShapesFile
  arbitrary = do
    drawSpaceTri <- arbitrary
    robotSpaceTri <- arbitrary
    paths <- arbitrary
    pure (ShapesFile {..})

jsonTests :: forall a. (Show a, Arbitrary a, Eq a, FromJSON a, ToJSON a) => Proxy a -> TestTree
jsonTests a =
  testGroup
    (show a <> " JSON serialization")
    [ QC.testProperty "encode should never produce empty json" $ \(x :: a) -> not . BS.null $ encode x,
      QC.testProperty ("decodes " <> show a <> " from JSON") $ \(x :: a) -> (decode (encode x) :: Maybe a) == Just x
    ]

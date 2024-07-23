module ShapesTests (shapesTests) where

import Data.Aeson
import Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Linear
import Shapes
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

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
    [ testCase "has the correct drawSpaceTri" $ do
        drawSpaceTri exampleFile @?= expectedDrawSpaceTri,
      testCase "has the correct robotSpaceTri" $ do
        robotSpaceTri exampleFile @?= expectedRobotSpaceTri,
      testCase "has the correct paths" $ do
        paths exampleFile @?= [expectedPath]
    ]
  where
    expectedDrawSpaceTri =
      V3
        (V2 0 0)
        (V2 0.5877852522924731 0.8090169943749475)
        (V2 0.5877852522924731 (-0.8090169943749473))
    expectedRobotSpaceTri =
      V3
        (V2 3.1622776601683795 9.869604401089358)
        (V2 2.718281828459045 1.0986122886681098)
        (V2 (-0.04321391826377225) (-0.5403023058681398))
    expectedPath =
      Path
        { trail =
            V2 0.5877852522924731 0.8090169943749475
              :| [ V2 0.9510565162951535 0.30901699437494745,
                   V2 0.9510565162951536 (-0.30901699437494734),
                   V2 0.5877852522924732 (-0.8090169943749473)
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
        toNGon 3 2 @?= (V2 1.2246467991473532e-16 (-1.0) :: V2 Double),
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
        toNGon 6 5 @?= (V2 (-0.1666666666666668) (-0.9660254037844385) :: V2 Double)
    ]

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

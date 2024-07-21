-- FILEPATH: /c:/Users/chris/OneDrive/Documents/GitHub/nolrai-scratch/test/ShapesSpec.hs

module ShapesSpec where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import Linear
import Shapes
import Test.Tasty
import Test.Tasty.Golden

shapesTests :: TestTree
shapesTests =
  testGroup
    "Shapes Tests"
    [ testGroup
        "exampleFile"
        [ testCase "has the correct drawSpaceTri" $ do
            let expectedDrawSpaceTri = V3 (V2 0 0) (V2 0.5877852522924731 0.8090169943749475) (V2 0.5877852522924731 (-0.8090169943749473))
            drawSpaceTri exampleFile @?= expectedDrawSpaceTri,
          testCase "has the correct robotSpaceTri" $ do
            let expectedRobotSpaceTri = V3 (V2 3.1622776601683795 9.869604401089358) (V2 2.718281828459045 1.0986122886681098) (V2 (-0.04321391826377225) (-0.5403023058681398))
            robotSpaceTri exampleFile @?= expectedRobotSpaceTri,
          testCase "has the correct paths" $ do
            let expectedPaths = [Path {trail = V2 0.5877852522924731 0.8090169943749475 :| [V2 0.9510565162951535 0.30901699437494745, V2 0.9510565162951536 (-0.30901699437494734), V2 0.5877852522924732 (-0.8090169943749473)], water = 10.0}]
            paths exampleFile @?= expectedPaths
        ],
      testGroup
        "toNGon"
        [ testCase "returns the correct points for a triangle" $ do
            toNGon 3 0 @?= V2 0.8660254037844386 0.5
            toNGon 3 1 @?= V2 (-0.8660254037844384) 0.5
            toNGon 3 2 @?= V2 1.2246467991473532e-16 (-1.0),
          testCase "returns the correct points for a pentagon" $ do
            toNGon 5 0 @?= V2 0.5877852522924731 0.8090169943749475
            toNGon 5 1 @?= V2 0.9510565162951535 0.30901699437494745
            toNGon 5 2 @?= V2 0.9510565162951536 (-0.30901699437494734)
            toNGon 5 3 @?= V2 0.5877852522924732 (-0.8090169943749473)
            toNGon 5 4 @?= V2 6.123233995736766e-17 (-1.0),
          testCase "returns the correct points for a hexagon" $ do
            toNGon 6 0 @?= V2 0.8660254037844386 0.5
            toNGon 6 1 @?= V2 0.49999999999999994 0.8660254037844386
            toNGon 6 2 @?= V2 (-0.16666666666666674) 0.9660254037844386
            toNGon 6 3 @?= V2 (-0.6666666666666667) 0.5
            toNGon 6 4 @?= V2 (-0.6666666666666667) (-0.5)
            toNGon 6 5 @?= V2 (-0.1666666666666668) (-0.9660254037844385)
        ]
    ]

shapesGolden :: TestTree
shapesGolden =
  testGroup
    "Shapes Golden Tests"
    [ goldenVsStringDiff
        "Encode the example ShapeFile datastructure."
        (\ref new -> ["diff", "-u", ref, new])
        "golden/exampleFile.shapes"
        pure
        (encode exampleFile)
    ]

shapesFileJsonTests :: TestTree
shapesFileJsonTests =
  testGroup
    "ShapesFile JSON serialization"
    [ testCase "encodes ShapesFile to JSON" $ do
        let json = encode exampleFile
        json `shouldSatisfy` (not . BS.null),
      testCase "decodes ShapesFile from JSON" $ do
        let json = encode exampleFile
        decode json @?= Just exampleFile
    ]

pathJsonTests :: TestTree
pathJsonTests =
  testGroup
    "Path JSON serialization"
    [ testCase "encodes Path to JSON" $ do
        let path = Path {trail = V2 0 0 :| [V2 1 1], water = 10.0}
        let json = encode path
        json `shouldSatisfy` (not . null),
      testCase "decodes Path from JSON" $ do
        let path = Path {trail = V2 0 0 :| [V2 1 1], water = 10.0}
        let json = encode path
        decode json @?= Just path
    ]

shapesTests =
  testGroup
    "Shapes.hs"
    [ shapesTests,
      shapesGolden,
      shapesFileJsonTests,
      pathJsonTests
    ]

shapesGolden =
  testGroup
    "Shapes Golden Tests"
    [ goldenVsStringDiff
        "Encode the example ShapeFile datastructure."
        (\ref new -> ["diff", "-u", ref, new])
        "golden/exampleFile.shapes"
        (pure $ encode exampleFile)
    ]

module Main (main) where

import ShapesTests
import Test.Tasty

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "All Tests"
      [ shapesTests
      ]

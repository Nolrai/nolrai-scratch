{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import ShapesSpec qualified

main :: IO ()
main = do
    defaultMain $ do
    testGroup
      "All Tests"
      [ shapesTests
      ]

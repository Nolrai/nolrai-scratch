module Main (main) where

import Boxes (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)

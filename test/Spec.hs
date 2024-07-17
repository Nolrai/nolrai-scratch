module Main (main) where

import Boxes (projectName)

main :: IO ()
main = putStrLn ("Tests for " ++ projectName)

data Coin = H | T

scoreAlice :: List -> Int
scoreAlice [] = 0
scoreAlice xs =
    case take 2 xs of
    

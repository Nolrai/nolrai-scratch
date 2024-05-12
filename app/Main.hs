{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude
import Data.Map ()
import Data.List as List ( drop, sort )
import Control.Monad ( guard )
import Text.Read (readMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  [fileName] <- getArgs
  result <- process <$> readFile fileName
  putStrLn result

process :: String -> String
process str =
    -- sort the words by the number, remove the number, and add seperating spaces 
    unwords $ snd <$> List.sort filtered 
  where
    filtered = do
      l <- lines str -- split into lines
      let (n', word) = break (== ' ') l-- break each line into the two parts
      let Just (n :: Int) = readMaybe n' -- discard if n' doesn't parse into a Int
      guard $ isTriangular n -- discard if n isn't triangular
      pure (n, List.drop 1 word) -- keep the n so we can sort the list

-- A the mth triangular number n = (m * (m+1). 
-- So solving for m -> m = ((sqrt (8n+1)) - 1)/2 
-- therefor n is triangular iff (8n + 1) is a pefect square
isTriangular :: Int -> Bool
isTriangular n = isPerfectSq (8 * n + 1)

isPerfectSq :: Int -> Bool
isPerfectSq n = n == test * test
  where
  test = floor (sqrt (fromIntegral n) :: Double)

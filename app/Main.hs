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
  [arg] <- getArgs
  

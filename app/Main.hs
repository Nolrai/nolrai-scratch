{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude
import Data.Map ()
import Data.List as List ( drop, sort )
import Control.Monad ( guard )
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.ByteString.Lazy as BS
import Data.Aeson
import Shapes
import Data.Aeson.Encode.Pretty

main :: IO ()
main = do
  BS.putStr $ encodePretty exampleFile <> "\n"

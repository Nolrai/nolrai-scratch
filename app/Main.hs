{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (guard)
import Data.ByteString.Char8 qualified as BS
import QSpice
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import Text.Megaparsec
import Prelude

main :: IO ()
main = do
  [path] <- getArgs
  fileContents <- BS.readFile path
  putStrLn $ "length of file = " <> show (BS.length fileContents)
  case parse schematicFile path fileContents of
    Left err -> die $ show err
    Right raw -> writeFile "out.txt" (show raw)
  exitSuccess

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens
import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Set as Set
import QSpice
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import Text.Megaparsec
import Prelude
import System.Directory

main :: IO ()
main = do
  [path] <- getArgs
  names <- body path
  putStrLn "Names used:"
  print `mapM_` getNames raw
  exitSuccess

body path = do
  isDir <- doesDirectoryExist path
  if isDir 
    then foldM (\ acc path -> body path `Set.union` acc) Set.empty =<< listDirectory path 
    else onSchematicFile path

onSchematicFile path = do
  fileContents <- BS.readFile path
  putStrLn $ "length of " <> BS.pack path <> " = " <> show (BS.length fileContents)
  case parse schematicFile path fileContents of
    Left err -> do
      hPutStrLn stderr "Errors:"
      hPutStrLn stderr $ errorBundlePretty err
    Right raw -> pure $ getNames raw 

getNames :: Raw -> Set BS.ByteString
getNames r = (r ^. name) `Set.insert` foldMap getNames (r ^. contents)

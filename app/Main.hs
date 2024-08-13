{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens
import Control.Monad (foldM)
import Data.Binary.Builder
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Set as Set
import Data.Text.Encoding
import QSpice
import System.Directory
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO
import Text.Megaparsec
import Prelude

main :: IO ()
main = do
  [path] <- getArgs
  names <- body path
  BS.putStrLn "Names used:"
  BS.putStrLn `mapM_` names
  exitSuccess

body :: FilePath -> IO (Set BS.ByteString)
body filename = do
  path <- makeAbsolute filename
  isDir <- doesDirectoryExist path
  if isDir
    then
      withCurrentDirectory path $
        foldM step Set.empty =<< listDirectory path
    else onSchematicFile path

step :: Set BS.ByteString -> FilePath -> IO (Set BS.ByteString)
step acc filename = (acc `Set.union`) <$> body filename

onSchematicFile :: FilePath -> IO (Set BS.ByteString)
onSchematicFile filename = do
  path <- makeAbsolute filename -- so error messages are more helpful!
  fileContents <- BS.readFile path
  BS.putStrLn $ "length of " <> BS.pack path <> " = " <> (BS.pack . show . BS.length) fileContents
  case parse schematicFile path fileContents of
    Left err -> do
      BS.hPutStrLn stderr "Errors:"
      BS.hPutStrLn stderr . BS.pack . errorBundlePretty $ err
      pure Set.empty
    Right schematic -> pure Set.empty

getNames :: Raw -> Set BS.ByteString
getNames r = (r ^. name) `Set.insert` foldMap getNames (r ^. contents)

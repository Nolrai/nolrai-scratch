{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans.Cont (Cont, cont, evalCont)
import Convert (shapesToFlowCmd)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BS hiding (length)
import Data.ByteString.Lazy.Char8 qualified as C8
import FlowCmd (FlowCmd, toGCode)
import Options (Options (..), Stage (..), fullParser)
import Options.Applicative (execParser)
import Shapes (Path (Path, trail), ShapesFile (paths), exampleFile)
import System.Exit (die, exitSuccess)
import System.IO
import Text.Printf (printf)
import Prelude

-- | The entry point of the program.
main :: IO ()
main = do
  options <- execParser fullParser
  evalCont $ main' options
  exitSuccess

-- | The entry point of the program.
--   Parses command line options, opens input and output files,
--   and executes the `body` function with the input and output
main' :: Options -> Cont (IO ()) (IO ())
main' Options {..} = do
  inputHandle <-
    case inputPath of
      "" -> pure stdin
      str -> cont (withFile str ReadMode)
  outputHandle <-
    case outputPath of
      "" -> pure stdout
      str -> cont (withFile str WriteMode)
  pure $ body inputHandle outputHandle inputStage outputStage

-- depending on the input and output stages,
-- read the input, log it to stderr, convert it to the output, log that, and write the output.
body :: Handle -> Handle -> Stage -> Stage -> IO ()
body inputHandle outputHandle inputStage outputStage = do
  inputStr <- BS.hGetContents inputHandle
  case inputStage of
    ShapesStage -> do
      inputShapes <-
        if BS.null inputStr
          then exampleFile <$ hPutStrLn stderr "Using internal example."
          else case eitherDecode inputStr of
            Left err -> die err
            Right value -> hPutStrLn stderr "Read Shapes file, and parsed JSON." >> pure value
      hPutStrLn stderr "Path lengths: "
      paths inputShapes `forM_` \Path {trail} ->
        hPutStrLn stderr $ printf "%8d" (length trail)

      if outputStage == ShapesStage
        then do
          BS.hPut outputHandle (encodePretty inputShapes <> "\n")
          hPutStrLn stderr "Wrote shapes as JSON to file"
        else do
          let flowList = shapesToFlowCmd inputShapes
          logFlowCmds flowList
          if outputStage == FlowCmdStage
            then outputFlowCmds outputHandle flowList
            else outputGCodes outputHandle (toGCode <$> flowList)
    FlowCmdStage -> do
      flowList <- (readIO . C8.unpack) `mapM` C8.lines inputStr
      logFlowCmds flowList
      when (outputStage == ShapesStage) $ die "Can't output a stage earlier than the input stage"
      if outputStage == FlowCmdStage
        then outputFlowCmds outputHandle flowList
        else outputGCodes outputHandle (toGCode <$> flowList)
    GCodeStage -> die "Parsing GCode not implemented."

-- write the gcodes to the handle
outputGCodes :: Handle -> [String] -> IO ()
outputGCodes outputHandle gcodes = do
  hPutStrLn outputHandle `mapM_` gcodes
  hPutStrLn stderr "Wrote FlowCmds as lines of gcode to file."

-- write the FlowCmds as newline deliminated haskell code
outputFlowCmds :: Handle -> [FlowCmd] -> IO ()
outputFlowCmds outputHandle flowList = do
  hPrint outputHandle `mapM_` flowList
  hPutStrLn stderr "Wrote FlowCmds as lines of Haskell text to file."

-- write info about the FlowCmds
logFlowCmds :: [FlowCmd] -> IO ()
logFlowCmds flowList = hPutStrLn stderr $ show (length flowList) <> " flowCmd computed."

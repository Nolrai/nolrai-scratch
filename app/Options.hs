{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Options where

import Prelude
import Text.Read (readMaybe)
import Options.Applicative
import Data.Char (toLower)
import Data.List (isPrefixOf, find)

fullParser :: ParserInfo Options
fullParser = info optionsParser parserInfoMods 

parserInfoMods :: InfoMod Options 
parserInfoMods 
  =   fulllDesc
  <>  progDesc "Convert flowbot shape files into gcode, with other options for debuging."


-- the central options parser
optionsParser :: Parser Options
optionsParser =
  Options <$> inputFiletypeOpt <*> outputFiletypeOpt <*> inputPathArg <*> outputPathArg

data Options = Options
  { inputStage :: Stage
  , outputStage :: Stage
  , outputPath :: FilePath
  , inputPath :: FilePath
  }

-- What should we process the input output as.
-- we don't actually handle reading GCodeStage
data Stage
  = ShapesStage
  | FlowCmdStage
  | GCodeStage
  deriving (Show, Read)

-- | Parses a 'Stage' from a command-line argument.
-- Matches the input string with one of the 'Stage' values, ignoring case.
readStage :: ReadM Stage
readStage = maybeReader readStage'
  where
  readStage' :: String -> Maybe Stage
  readStage' str = find (go str) [ShapesStage, FlowCmdStage, GCodeStage]
    where
      go input stage = map toLower input `isPrefixOf` map toLower (show stage)

-- | Command-line option parser for the input file type.
-- Defaults to 'ShapesStage' if no input is provided.
inputFiletypeOpt :: Parser Stage
inputFiletypeOpt = option readStage
  ( long "input"
  <> short 'i'
  <> metavar "STAGE"
  <> value ShapesStage
  <> showDefault
  <> help "What format the input is expected in, must be either Shapes or FlowCmd"
  )

-- | Command-line option parser for the output file type.
-- Defaults to 'GCodeStage' if no input is provided.
outputFiletypeOpt :: Parser Stage
outputFiletypeOpt = option readStage
  ( long "output"
  <> short 'o'
  <> metavar "STAGE"
  <> value GCodeStage
  <> showDefault
  <> help "What format to output, must be one of Shapes, FlowCmd, or GCode, and must be later than or the same as the input stage"
  )

-- | Command-line argument parser for the output file path.
-- Defaults to an empty string if no input is provided.
outputPathArg :: Parser FilePath
outputPathArg = strArgument (value "" <> metavar "FILEPATH" <> help "Input file, if omitted stdin will be used.")

-- | Command-line argument parser for the input file path.
-- Defaults to an empty string if no input is provided.
inputPathArg :: Parser FilePath
inputPathArg = strArgument (value "" <> metavar "FILEPATH" <> help "Output file, if omitted stdout will be used.")

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QSpice where

import Control.Lens
import Control.Monad (guard)
import Data.ByteString.Lazy (ByteString, pack)
import Data.ByteString.Lazy qualified as BS
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.Functor
import GHC.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as L
import Data.Text
import Linear.V2

type Parser = Parsec () ByteString

instance ShowErrorComponent () where
  showErrorComponent :: () -> String
  showErrorComponent () = ""
  errorComponentLen :: () -> Int
  errorComponentLen () = 0

data Raw = Raw
  { rawName :: ByteString,
    rawArgs :: [ByteString],
    lable :: Text,
    rawProperties :: [Property],
    rawContents :: [Raw]
  }
  deriving (Show)

data Property = Property
  { propertyName :: ByteString,
    propertyValue :: ByteString
  }
  deriving (Show)

makeFields ''Property
makeFields ''Raw

schematicFile :: Parser Raw
schematicFile = string "ÿØÿÛ" *> angleWrap parseRaw

whiteSpace :: Parser ()
whiteSpace = L.space space1 empty empty

symbol :: ByteString -> Parser ByteString
symbol = L.symbol whiteSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

angleOpen, angleClose :: Token ByteString
angleOpen = 171
angleClose = 187

isAngleChar :: Word8 -> Bool
isAngleChar c = (c == angleOpen) || (c == angleClose)

openTag :: Parser ()
openTag = lexeme (single angleOpen) $> ()

closeTag :: Parser ()
closeTag = lexeme (single angleClose) $> ()

angleWrap :: Parser a -> Parser a
angleWrap p = try (openTag *> p <* closeTag)

consP :: Parser Word8 -> Parser ByteString -> Parser ByteString
start `consP` body = BS.cons <$> start <*> body

name' :: Parser ByteString
name' = letterChar `consP` (BS.pack <$> some (alphaNumChar :: Parser (Token ByteString)))

data ABC a b c = A a | B b | C c

abcSplit :: [ABC a b c] -> ([a], [b], [c])
abcSplit [] = ([], [], [])
abcSplit (x : xs) = f x $ abcSplit xs
  where
    f :: ABC a b c -> ([a], [b], [c]) -> ([a], [b], [c])
    f (A a) = over _1 (a :)
    f (B b) = over _2 (b :)
    f (C c) = over _3 (c :)

parseRaw :: Parser Raw
parseRaw = do
  n <- parseName
  a <- argList
  (p, c) <- angleList
  pure $ Raw n a p c

parseName :: Parser ByteString
parseName = label "name" $ lexeme name'

argList :: Parser [ByteString]
argList = many (lexeme parseArg)

data Arg 
  = Pos (V2 Double)
  | Value Double
  | Label Text
  | Ident ByteString
  | Image ByteString

parseArg =
  parsePos <|> parseValue <|> parseLable <|> parse

angleList :: Parser ([Property], [Raw])
angleList = partitionEithers <$> many angleItem

angleItem = angleWrap ((Left <$> try parseProperty) <|> (Right <$> parseRaw))

parseProperty :: Parser Property
parseProperty =
  label "property" $
    Property
      <$> parsePropName
      <*> untilAngle

parsePropName :: Parser ByteString
parsePropName = lexeme $ name' <* string ":"

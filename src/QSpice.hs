{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QSpice where

import Control.Concurrent (yield)
import Control.Lens
import Control.Monad (guard)
import Data.ByteString.Lazy (ByteString, pack)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr, isHexDigit, isLetter, ord)
import Data.Either (partitionEithers)
import Data.Functor
import GHC.Word (Word8)
import Linear.V2
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer (decimal, float)
import Text.Megaparsec.Byte.Lexer qualified as L

type Parser = Parsec () ByteString

instance ShowErrorComponent () where
  showErrorComponent :: () -> String
  showErrorComponent () = ""
  errorComponentLen :: () -> Int
  errorComponentLen () = 0

data Raw = Raw
  { rawName :: ByteString,
    rawArgs :: [ByteString],
    rawProperties :: [Property],
    rawContents :: [Raw]
  }
  deriving (Show)

data Property = Property
  { propertyName :: ByteString,
    propertyValue :: ByteString
  }
  deriving (Show)

data Arg
  = Pos (V2 Double)
  | Value Double
  | Label ByteString
  | Ident ByteString
  | Image ByteString
  deriving (Show)

makeClassy ''Arg
makeFields ''Property
makeFields ''Raw

schematicFile :: Parser Raw
schematicFile = string "ÿØÿÛ" *> angleWrap parseRaw

whiteSpace :: Parser ()
whiteSpace = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

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
parseName = lexeme name' <?> "name"

argList :: Parser [Arg]
argList = many (lexeme parseArg)

parseArg :: Parser Arg
parseArg = (parseImage <|> parsePos <|> parseValue <|> parseLabel <|> parseIdent) <?> "argument"

parsePos, parseValue, parseLabel, parseImage :: Parser Arg
parsePos = lexeme $ do
  symbol "("
  x <- float
  symbol ","
  y <- float
  symbol ")"
  pure . Pos $ V2 x y
parseValue = try . lexeme $ Value <$> float
parseLabel = lexeme $ do
  quoteMark
  str <- takeWhileP Nothing ((/= '"') . toChar)
  quoteMark
  pure $ Label str

parseIdent = lexeme $ Ident <$> takeWhile1P (Just "letter") (isLetter . toChar)

parseImage = try . lexeme $ do
  hexDigits <- takeWhileP (Just "Hex Digit") (isHexDigit . toChar)
  let n = BS.length hexDigits
  guard $ (n >= 80) && even n
  pure $ Image hexDigits

quoteMark :: Parser ()
quoteMark = symbol "\"" $> ()

angleList :: Parser ([Property], [Raw])
angleList = partitionEithers <$> many angleItem

angleItem :: Parser (Either Property Raw)
angleItem = angleWrap ((Left <$> try parseProperty) <|> (Right <$> parseRaw))

parseProperty :: Parser Property
parseProperty =
  (<?> "property") $
    Property
      <$> parsePropName
      <*> takeWhile1P (Just "property value") (not . isAngleChar)

parsePropName :: Parser ByteString
parsePropName = lexeme $ name' <* string ":"

toChar :: Word8 -> Char
toChar = chr . fromIntegral

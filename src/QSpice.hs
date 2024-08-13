{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module QSpice where

import Control.Applicative (Alternative)
import Control.Concurrent (yield)
import Control.Lens hiding (noneOf)
import Control.Monad (guard, when)
import Data.ByteString.Lazy (ByteString, pack)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr, isAlpha, isAlphaNum, isHexDigit, isLetter, isSpace, ord)
import Data.Either (partitionEithers)
import Data.Functor
import Data.List as List
import Data.Maybe (fromMaybe)
import Debug.Trace (traceM)
import GHC.Word (Word32, Word8)
import Linear.V2
import Linear.V3
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as L

type Parser = Parsec () ByteString

instance ShowErrorComponent () where
  showErrorComponent :: () -> String
  showErrorComponent () = ""
  errorComponentLen :: () -> Int
  errorComponentLen () = 0

data Raw = Raw
  { rawName :: ByteString,
    rawArgs :: [Arg],
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
  | Double Double
  | Hex Integer
  | Label ByteString
  | Ident ByteString
  | Image ByteString
  deriving (Show)

makeClassy ''Arg
makeFields ''Property
makeFields ''Raw

schematicFileToRaw :: Parser Raw
schematicFileToRaw = string "ÿØÿÛ" *> angleWrap parseRaw

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
openTag = lexeme (single angleOpen) $> () <?> "open tag"

closeTag :: Parser ()
closeTag = lexeme (single angleClose) $> () <?> "close tag"

angleWrap :: Parser a -> Parser a
angleWrap p = try (openTag *> p <* closeTag)

name' :: Parser ByteString
name' = do
  start <- satisfy (isAlpha . toChar)
  body <- takeWhileP Nothing (isAlphaNum . toChar)
  pure $ BS.cons start body

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
  !n <- parseName
  !args <- argList
  !props <- many . angleWrap $ parseProperty
  !subRaws <- many . angleWrap $ parseRaw
  pure $ Raw n args props subRaws

parseName :: Parser ByteString
parseName = name' <* space1 <?> "name"

argList :: Parser [Arg]
argList = many parseArg

parseArg :: Parser Arg
parseArg = parseImage <|> parsePos <|> parseDouble <|> parseHex <|> parseLabel <|> parseIdent

float :: ParsecT () ByteString Identity Double
float = L.signed (pure ()) $ try L.float <|> fromIntegral <$> L.decimal

parsePos, parseIdent, parseDouble, parseHex, parseLabel, parseImage :: Parser Arg
parsePos = lexeme $ do
  symbol "("
  x <- float
  symbol ","
  y <- float
  symbol ")"
  pure . Pos $ V2 x y
parseDouble = try . lexeme $ Double <$> float
parseHex = try (string "0x") *> lexeme (Hex <$> L.hexadecimal)
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
parsePropName = takeWhile1P (Just "alphanumeric or space") f <* lexeme (string ":")
  where
    f w = let c = toChar w in isAlphaNum c || isSpace c

toChar :: Word8 -> Char
toChar = chr . fromIntegral

newtype Schematic = Schematic [SchematicItem]

data SchematicItem
  = -- «Arc3p (0,-300) (0,300) (0,0) 0   0   0x1000000 -1  -1»
    Arc3p (V3 (V2 Int)) Int Int Word32 Int Int
  | -- «coil  (-80,180) (80,-180) 0   0   0   0x1000000 -1  -1»
    Coil (V2 Int) (V2 Int) Int Int Int Word32 Int Int
  | -- «component (-400,600) 3 0
    Component (V2 Int) Int Int Symbol
  | -- «Ellipse   (-150,-150) (150,150) 0   0   0   0x1000000 0x1000000 -1  -1»
    Ellipse (V2 Int) (V2 Int) Int Int Int Word32 Word32 Int Int
  | -- «Junction  (-1100,-400)»
    Junction (V2 Int)
  | -- «line  (0,200)   (0,80)    0   0   0x1000000 -1  -1»
    Line (V2 Int) (V2 Int) Int Int Word32 Int Int
  | -- «net   (15900,-2300) 1   14  0   "GND"»
    Net (V2 Int) Int Int Int ByteString
  | -- «pin   (0,200)  (0,0)     1   0   0   0x0   -1  "K"»
    Pin (V2 Int) (V2 Int) Double Int Int Word32 Int [ByteString]
  | -- «rect  (-130,30) (130,40)  0   0   0   0x1000000 0x3000000 -1  0   -1»
    Rect (V2 Int) (V2 Int) Int Int Int Word32 Word32 Int Int Int (Maybe ByteString)
  | -- «text (150,-150) 1   75  0   0x1000000 -1  -1  "D5"»
    Text (V2 Int) Double Int Int Word32 Int Int ByteString
  | -- «triangle (200,100) (178,88) (175,100) 0   0   0x1000000 0x3000000 -1  -1»
    Triangle (V3 (V2 Int)) Int Int Word32 Word32 Int Int
  | -- «wire (13900,-2800) (13900,-2600) "GND"»
    Wire (V2 Int) (V2 Int) ByteString
  | -- «zigzag (-80,180) (80,-180) 0   0   0    0x1000000 -1  -1»
    Zigzag (V2 Int) (V2 Int) Int Int Int Word32 Int Int
  deriving (Show)

--           «symbol D
data Symbol = Symbol (Maybe ByteString) [Property] [SchematicItem]
  deriving (Show)

schematicFile :: Parser Schematic
schematicFile = do
  string "ÿØÿÛ"
  openTag >> symbol "schematic"
  list <- some (try parseSchematicItem)
  closeTag
  pure $ Schematic list

parseSchematicItem :: Parser SchematicItem
parseSchematicItem = do
  openTag
    *> tryThese
      [ arc3p,
        coil,
        component,
        ellipse,
        junction,
        line,
        net,
        pin,
        rect,
        spiceText,
        triangle,
        wire,
        zigzag
      ]
    <* closeTag

arc3p,
  coil,
  component,
  ellipse,
  junction,
  line,
  net,
  pin,
  rect,
  spiceText,
  triangle,
  wire,
  zigzag ::
    Parser SchematicItem
tryThese :: [Parser a] -> Parser a
tryThese = List.foldl' (\acc next -> try next <|> acc) empty

decimal' :: Parser Int
decimal' = L.signed (pure ()) L.decimal

decimal :: Parser Int
decimal = lexeme decimal'

word32Hex :: Parser Word32
word32Hex = lexeme $ symbol "0x" *> L.hexadecimal

word8Hex :: Parser Word8
word8Hex = lexeme . (<?> "decimal value in [0, 255]") $
  do
    string "0x"
    (v :: Integer) <- L.hexadecimal
    when (v < 0 || 255 < v) (fail $ show v <> " is outside of word8 range")
    pure $ fromInteger v

position :: Parser (V2 Int)
position = V2 <$> (symbol "(" *> decimal') <*> (symbol "," *> decimal' <* symbol ")")

threeByTwo :: Parser (V3 (V2 Int))
threeByTwo = V3 <$> lexeme position <*> lexeme position <*> lexeme position

arc3p = do
  symbol "arc3p"
  Arc3p
    <$> threeByTwo
    <*> decimal
    <*> decimal
    <*> word32Hex
    <*> decimal
    <*> decimal

coil = do
  symbol "coil"
  Coil
    <$> lexeme position
    <*> lexeme position
    <*> decimal
    <*> decimal
    <*> decimal
    <*> word32Hex
    <*> decimal
    <*> decimal

component = do
  symbol "component"
  Component
    <$> lexeme position
    <*> decimal
    <*> decimal
    <*> (openTag *> spiceSymbol <* closeTag)

spiceSymbol :: ParsecT () ByteString Identity Symbol
spiceSymbol = do
  symbol "symbol"
  Symbol
    <$> optional (lexeme name')
    <*> propList
    <*> some (try parseSchematicItem)

ellipse = do
  symbol "ellipse"
  Ellipse
    <$> lexeme position
    <*> lexeme position
    <*> decimal
    <*> decimal
    <*> decimal
    <*> lexeme word32Hex
    <*> lexeme word32Hex
    <*> decimal
    <*> decimal

junction = do
  symbol "junction"
  Junction
    <$> lexeme position

line = do
  symbol "line"
  Line
    <$> lexeme position
    <*> lexeme position
    <*> decimal
    <*> decimal
    <*> lexeme word32Hex
    <*> decimal
    <*> decimal

quotedString :: Parser ByteString
quotedString =
  quoteMark *> takeWhileP Nothing ((/= '"') . toChar) <* quoteMark

net = do
  symbol "net"
  Net
    <$> lexeme position
    <*> decimal
    <*> decimal
    <*> decimal
    <*> quotedString

pin = do
  symbol "pin"
  Pin
    <$> lexeme position
    <*> lexeme position
    <*> lexeme float
    <*> decimal
    <*> decimal
    <*> word32Hex
    <*> decimal
    <*> some (lexeme quotedString)

rect = do
  symbol "rect"
  Rect
    <$> lexeme position
    <*> lexeme position
    <*> decimal
    <*> decimal
    <*> decimal
    <*> word32Hex
    <*> word32Hex
    <*> decimal
    <*> decimal
    <*> decimal
    <*> optional (takeWhile1P (Just "hex digit") (isHexDigit . toChar) <* quoteMark)

spiceText = do
  symbol "text"
  Text
    <$> lexeme position
    <*> lexeme float
    <*> decimal
    <*> decimal
    <*> lexeme word32Hex
    <*> decimal
    <*> decimal
    <*> quotedString

triangle = do
  symbol "triangle"
  Triangle
    <$> threeByTwo
    <*> decimal
    <*> decimal
    <*> lexeme word32Hex
    <*> lexeme word32Hex
    <*> decimal
    <*> decimal

wire = do
  symbol "wire"
  Wire
    <$> lexeme position
    <*> lexeme position
    <*> (quoteMark *> takeWhileP Nothing ((/= '"') . toChar) <* quoteMark)

zigzag = do
  symbol "zigzag"
  Zigzag
    <$> lexeme position
    <*> lexeme position
    <*> decimal
    <*> decimal
    <*> decimal
    <*> lexeme word32Hex
    <*> decimal
    <*> decimal

propList :: Parser [Property]
propList = many (angleWrap parseProperty)

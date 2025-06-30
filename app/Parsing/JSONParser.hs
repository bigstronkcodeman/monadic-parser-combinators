{-# LANGUAGE OverloadedStrings #-}

module Parsing.JSONParser (
  JsonValue (..),
  jsonValue,
  prettyPrintJson
) where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Control.Monad.Fail (MonadFail(fail))
import Data.Char (chr)
import Data.Functor (($>))
import Data.HashMap.Lazy (HashMap, fromList, toList)
import Data.Text.Lazy (Text, pack, unpack)
import Data.List (intercalate)
import Numeric (readHex)
import Parsing.Combinators (sepBy)
import Parsing.ParserTypes (Parser)
import Parsing.Primitives (takeWhile, (<?>))
import Parsing.UtilParsers (char, float, text, satisfy)
import Prelude hiding (takeWhile, fail)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString Text
  | JsonArray [JsonValue]
  | JsonObject (HashMap Text JsonValue)
  deriving (Show, Eq)

jsonWhitespace :: Parser ()
jsonWhitespace =
  void (takeWhile $ \c ->
      c == ' '
        || c == '\n'
        || c == '\r'
        || c == '\t')
    <?> "jsonWhitespace"

jsonToken :: Parser a -> Parser a
jsonToken p =
  ((jsonWhitespace *> p) <* jsonWhitespace)
    <?> "jsonToken"

jsonKey :: Parser Text
jsonKey =
  ( do
      _ <- char '"'
      content <- many stringChar
      _ <- char '"'
      return (pack content)
  )
    <?> "jsonKey"

stringChar :: Parser Char
stringChar = escapedChar <|> normalChar
  <?> "stringChar"

normalChar :: Parser Char
normalChar = satisfy (\c -> c /= '"' && c /= '\\' && c >= '\x20')
  <?> "normalChar"

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  c <- satisfy (`elem` ("\"\\/bfnrtu" :: String))
  case c of
    '"'  -> return '"'
    '\\' -> return '\\'
    '/'  -> return '/'
    'b'  -> return '\b'
    'f'  -> return '\f'
    'n'  -> return '\n'
    'r'  -> return '\r'
    't'  -> return '\t'
    'u'  -> unicodeEscape
    _    -> fail "Invalid escape sequence"
  <?> "escapedChar"

unicodeEscape :: Parser Char
unicodeEscape = do
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  let hexStr = [d1, d2, d3, d4]
  case readHex hexStr of
    [(n, "")] -> return (chr n)
    _         -> fail "Invalid unicode escape"
  <?> "unicodeEscape"

hexDigit :: Parser Char
hexDigit = satisfy (\c -> (c >= '0' && c <= '9') || 
                          (c >= 'a' && c <= 'f') || 
                          (c >= 'A' && c <= 'F'))
  <?> "hexDigit"

jsonKeyValuePair :: Parser (Text, JsonValue)
jsonKeyValuePair =
  ( do
      key <- jsonKey
      _ <- jsonToken (char ':')
      value <- jsonValue
      return (key, value)
  )
    <?> "jsonKeyValuePair"

jsonNull :: Parser JsonValue
jsonNull =
  (text "null" $> JsonNull)
    <?> "jsonNull"

jsonBool :: Parser JsonValue
jsonBool =
  (JsonBool . (== "true") <$> (text "true" <|> text "false"))
    <?> "jsonBool"

jsonNumber :: Parser JsonValue
jsonNumber =
  fmap JsonNumber float
    <?> "jsonNumber"

jsonString :: Parser JsonValue
jsonString =
  fmap JsonString jsonKey
    <?> "jsonString"

jsonArray :: Parser JsonValue
jsonArray =
  ( do
      _ <- char '['
      vs <- jsonToken jsonValue `sepBy` jsonToken (char ',')
      _ <- char ']'
      return $ JsonArray vs
  )
    <?> "jsonArray"

jsonObject :: Parser JsonValue
jsonObject =
  ( do
      _ <- char '{'
      kvps <- jsonToken jsonKeyValuePair `sepBy` jsonToken (char ',')
      _ <- char '}'
      return (JsonObject $ fromList kvps)
  )
    <?> "jsonObject"

jsonValue :: Parser JsonValue
jsonValue =
  ( jsonNull
      <|> jsonBool
      <|> jsonNumber
      <|> jsonString
      <|> jsonArray
      <|> jsonObject
  )
    <?> "jsonValue"

prettyPrintJson :: JsonValue -> String
prettyPrintJson = prettyPrintJsonIndent 0

prettyPrintJsonIndent :: Int -> JsonValue -> String
prettyPrintJsonIndent indent value = case value of
  JsonNull -> "null"
  JsonBool True -> "true"
  JsonBool False -> "false"
  JsonNumber n -> show n
  JsonString t -> "\"" ++ escapeForDisplay (unpack t) ++ "\""
  JsonArray [] -> "[]"
  JsonArray xs -> 
    "[\n" ++ 
    intercalate ",\n" (map (\x -> replicate (indent + 2) ' ' ++ prettyPrintJsonIndent (indent + 2) x) xs) ++
    "\n" ++ replicate indent ' ' ++ "]"
  JsonObject obj -> 
    let pairs = toList obj
    in if null pairs 
       then "{}"
       else "{\n" ++
            intercalate ",\n" (map (\(k, v) -> 
              replicate (indent + 2) ' ' ++ 
              "\"" ++ escapeForDisplay (unpack k) ++ "\": " ++ 
              prettyPrintJsonIndent (indent + 2) v) pairs) ++
            "\n" ++ replicate indent ' ' ++ "}"

escapeForDisplay :: String -> String
escapeForDisplay = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c 
      | c < ' ' = "\\u" ++ pad4 (showHex (fromEnum c) "")
      | otherwise = [c]  
    
    pad4 s = replicate (4 - length s) '0' ++ s
    showHex n s = case n of
      0 -> '0':s
      _ -> showHex (n `div` 16) (hexDigitChar (n `mod` 16) : s)
    
    hexDigitChar n 
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'a' + n - 10)

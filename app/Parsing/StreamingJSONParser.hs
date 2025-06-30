{-# LANGUAGE OverloadedStrings #-}

module Parsing.StreamingJSONParser (
  streamingJsonValue
) where

import Control.Applicative ((<|>), many)
import Control.Monad (void, mapM_)
import Control.Monad.Fail (MonadFail(fail))
import Data.Char (chr)
import Data.Functor (($>))
import Data.HashMap.Lazy (HashMap, fromList)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy
import Numeric (readHex)
import Parsing.Combinators (sepBy)
import Parsing.JSONParser (JsonValue(..))
import Parsing.ParserTypes (Parser)
import Parsing.StreamingPrimitives (streamingItem, streamingTakeWhile)
import Parsing.UtilParsers (satisfy)
import Prelude hiding (takeWhile, fail, mapM_)

streamingJsonValue :: Parser JsonValue
streamingJsonValue = 
  streamingJsonNull <|> 
  streamingJsonBool <|> 
  streamingJsonNumber <|> 
  streamingJsonString <|> 
  streamingJsonArray <|> 
  streamingJsonObject

streamingJsonNull :: Parser JsonValue
streamingJsonNull = do
  _ <- streamingChar 'n'
  _ <- streamingChar 'u' 
  _ <- streamingChar 'l'
  _ <- streamingChar 'l'
  return JsonNull

streamingJsonBool :: Parser JsonValue
streamingJsonBool = 
  (streamingText "true" $> JsonBool True) <|>
  (streamingText "false" $> JsonBool False)

streamingJsonNumber :: Parser JsonValue
streamingJsonNumber = do
  digits <- streamingTakeWhile1 (\c -> c >= '0' && c <= '9' || c == '.' || c == '-')
  case reads (Data.Text.Lazy.unpack digits) of
    [(n, "")] -> return (JsonNumber n)
    _ -> fail "Invalid number"

streamingJsonString :: Parser JsonValue
streamingJsonString = JsonString <$> streamingJsonKey

streamingJsonKey :: Parser Text
streamingJsonKey = do
  _ <- streamingChar '"'
  content <- many streamingStringChar
  _ <- streamingChar '"'
  return (pack content)

streamingStringChar :: Parser Char
streamingStringChar = 
  streamingEscapedChar <|> 
  satisfy (\c -> c /= '"' && c /= '\\' && c >= '\x20')

streamingEscapedChar :: Parser Char
streamingEscapedChar = do
  _ <- streamingChar '\\'
  c <- streamingItem
  case c of
    '"'  -> return '"'
    '\\' -> return '\\'
    '/'  -> return '/'
    'b'  -> return '\b'
    'f'  -> return '\f'
    'n'  -> return '\n'
    'r'  -> return '\r'
    't'  -> return '\t'
    'u'  -> streamingUnicodeEscape
    _    -> fail "Invalid escape sequence"

streamingUnicodeEscape :: Parser Char
streamingUnicodeEscape = do
  d1 <- streamingHexDigit
  d2 <- streamingHexDigit
  d3 <- streamingHexDigit
  d4 <- streamingHexDigit
  let hexStr = [d1, d2, d3, d4]
  case readHex hexStr of
    [(n, "")] -> return (chr n)
    _         -> fail "Invalid unicode escape"

streamingHexDigit :: Parser Char
streamingHexDigit = satisfy (\c -> (c >= '0' && c <= '9') || 
                                   (c >= 'a' && c <= 'f') || 
                                   (c >= 'A' && c <= 'F'))

streamingJsonArray :: Parser JsonValue
streamingJsonArray = do
  _ <- streamingChar '['
  streamingJsonWhitespace
  values <- streamingJsonValue `sepBy` (streamingJsonWhitespace *> streamingChar ',' <* streamingJsonWhitespace)
  streamingJsonWhitespace
  _ <- streamingChar ']'
  return (JsonArray values)

streamingJsonObject :: Parser JsonValue
streamingJsonObject = do
  _ <- streamingChar '{'
  streamingJsonWhitespace
  pairs <- streamingKeyValuePair `sepBy` (streamingJsonWhitespace *> streamingChar ',' <* streamingJsonWhitespace)
  streamingJsonWhitespace
  _ <- streamingChar '}'
  return (JsonObject $ fromList pairs)

streamingKeyValuePair :: Parser (Text, JsonValue)
streamingKeyValuePair = do
  key <- streamingJsonKey
  streamingJsonWhitespace
  _ <- streamingChar ':'
  streamingJsonWhitespace
  value <- streamingJsonValue
  return (key, value)

streamingChar :: Char -> Parser Char
streamingChar expected = do
  c <- streamingItem
  if c == expected
    then return c
    else fail $ "Expected " ++ show expected ++ " but got " ++ show c

streamingText :: Text -> Parser Text
streamingText txt = do
  mapM_ streamingChar (Data.Text.Lazy.unpack txt)
  return txt

streamingTakeWhile1 :: (Char -> Bool) -> Parser Text
streamingTakeWhile1 p = do
  first <- satisfy p
  rest <- streamingTakeWhile p
  return (Data.Text.Lazy.cons first rest)

streamingJsonWhitespace :: Parser ()
streamingJsonWhitespace = void $ streamingTakeWhile $ \c ->
  c == ' ' || c == '\n' || c == '\r' || c == '\t'

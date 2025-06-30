{-# LANGUAGE OverloadedStrings #-}

module Parsing.TrueStreaming (
  StreamingParser(..),
  StreamResult(..),
  parseIncrementally,
  streamingChar,
  streamingMany,
  streamingJsonValue
) where

import Control.Applicative ((<|>), many, some, Alternative(..))
import Control.Monad (void)
import Control.Monad.Fail (MonadFail(fail))
import Data.Char (chr)
import Data.Functor (($>))
import Data.HashMap.Lazy (HashMap, fromList)
import Data.Text.Lazy (Text, pack, unpack, null, uncons)
import qualified Data.Text.Lazy as TL
import Numeric (readHex)
import Parsing.JSONParser (JsonValue(..))
import Prelude hiding (fail, null)

data StreamResult a 
  = StreamDone Text a           
  | StreamPartial (Text -> StreamResult a)  
  | StreamFailed String         

instance (Show a) => Show (StreamResult a) where
  show (StreamDone remaining result) = "StreamDone " ++ show (unpack remaining) ++ " " ++ show result
  show (StreamPartial _) = "StreamPartial <continuation>"
  show (StreamFailed err) = "StreamFailed " ++ show err

newtype StreamingParser a = StreamingParser {
  runStreamingParser :: Text -> StreamResult a
}

instance Functor StreamingParser where
  fmap f (StreamingParser p) = StreamingParser $ \input ->
    case p input of
      StreamDone remaining result -> StreamDone remaining (f result)
      StreamPartial cont -> StreamPartial (\more -> runStreamingParser (fmap f (StreamingParser (\_ -> cont more))) TL.empty)
      StreamFailed err -> StreamFailed err

instance Applicative StreamingParser where
  pure x = StreamingParser $ \input -> StreamDone input x
  
  StreamingParser pf <*> StreamingParser px = StreamingParser $ \input ->
    case pf input of
      StreamDone remaining1 f -> 
        case px remaining1 of
          StreamDone remaining2 x -> StreamDone remaining2 (f x)
          StreamPartial cont -> StreamPartial (\more -> runStreamingParser (pure f <*> StreamingParser (\_ -> cont more)) TL.empty)
          StreamFailed err -> StreamFailed err
      StreamPartial cont -> StreamPartial (\more -> runStreamingParser (StreamingParser (\_ -> cont more) <*> StreamingParser px) TL.empty)
      StreamFailed err -> StreamFailed err

instance Alternative StreamingParser where
  empty = StreamingParser $ \_ -> StreamFailed "empty"
  
  StreamingParser p1 <|> StreamingParser p2 = StreamingParser $ \input ->
    case p1 input of
      StreamFailed _ -> p2 input
      success -> success

instance Monad StreamingParser where
  StreamingParser p >>= f = StreamingParser $ \input ->
    case p input of
      StreamDone remaining result -> runStreamingParser (f result) remaining
      StreamPartial cont -> StreamPartial (\more -> runStreamingParser (StreamingParser (\_ -> cont more) >>= f) TL.empty)
      StreamFailed err -> StreamFailed err

instance MonadFail StreamingParser where
  fail err = StreamingParser $ \_ -> StreamFailed err

parseIncrementally :: StreamingParser a -> Text -> StreamResult a
parseIncrementally = runStreamingParser

streamingChar :: Char -> StreamingParser Char
streamingChar expected = StreamingParser $ \input ->
  case uncons input of
    Nothing -> StreamPartial $ \more ->
      if null more 
        then StreamPartial (\evenMore -> runStreamingParser (streamingChar expected) evenMore)
        else runStreamingParser (streamingChar expected) more
    Just (c, rest) ->
      if c == expected
        then StreamDone rest c
        else StreamFailed $ "Expected " ++ show expected ++ " but got " ++ show c

streamingAnyChar :: StreamingParser Char
streamingAnyChar = StreamingParser $ \input ->
  case uncons input of
    Nothing -> StreamPartial $ \more ->
      if null more
        then StreamPartial (\evenMore -> runStreamingParser streamingAnyChar evenMore)
        else runStreamingParser streamingAnyChar more
    Just (c, rest) -> StreamDone rest c

streamingMany :: StreamingParser a -> StreamingParser [a]
streamingMany p = StreamingParser $ \input ->
  let go acc inp = case runStreamingParser p inp of
        StreamDone remaining result -> go (result:acc) remaining
        StreamPartial cont -> StreamPartial (\more -> runStreamingParser (StreamingParser (\_ -> go acc (inp `TL.append` more))) TL.empty)
        StreamFailed _ -> StreamDone inp (reverse acc)
  in go [] input

streamingMany1 :: StreamingParser a -> StreamingParser [a]
streamingMany1 p = do
  first <- p
  rest <- streamingMany p
  return (first : rest)

streamingString :: String -> StreamingParser String
streamingString [] = pure []
streamingString (c:cs) = do
  _ <- streamingChar c
  _ <- streamingString cs
  return (c:cs)

streamingWhitespace :: StreamingParser ()
streamingWhitespace = StreamingParser $ \input -> StreamDone (TL.dropWhile (`elem` (" \t\n\r" :: String)) input) ()

streamingJsonValue :: StreamingParser JsonValue
streamingJsonValue = streamingWhitespace *> (
  streamingJsonNull <|>
  streamingJsonBool <|>
  streamingJsonNumber <|>
  streamingJsonString <|>
  streamingJsonArray <|>
  streamingJsonObject
  ) <* streamingWhitespace

streamingJsonNull :: StreamingParser JsonValue
streamingJsonNull = do
  _ <- streamingString "null"
  return JsonNull

streamingJsonBool :: StreamingParser JsonValue
streamingJsonBool = 
  (streamingString "true" $> JsonBool True) <|>
  (streamingString "false" $> JsonBool False)

streamingJsonNumber :: StreamingParser JsonValue
streamingJsonNumber = do
  digits <- streamingMany1 digitOrDot
  case reads digits of
    [(n, "")] -> return (JsonNumber n)
    _ -> fail "Invalid number"
  where
    digitOrDot = StreamingParser $ \input ->
      case uncons input of
        Nothing -> StreamPartial $ \more ->
          if null more
            then StreamFailed "Unexpected EOF in number"
            else runStreamingParser digitOrDot more
        Just (c, rest) ->
          if c >= '0' && c <= '9' || c == '.' || c == '-'
            then StreamDone rest c
            else StreamFailed $ "Expected digit but got " ++ show c

streamingJsonString :: StreamingParser JsonValue
streamingJsonString = JsonString . pack <$> streamingJsonStringContent

streamingJsonStringContent :: StreamingParser String
streamingJsonStringContent = do
  _ <- streamingChar '"'
  content <- streamingMany streamingStringChar
  _ <- streamingChar '"'
  return content

streamingStringChar :: StreamingParser Char
streamingStringChar = streamingEscapedChar <|> streamingNormalChar
  where
    streamingNormalChar = StreamingParser $ \input ->
      case uncons input of
        Nothing -> StreamPartial $ \more ->
          if null more
            then StreamPartial (\evenMore -> runStreamingParser streamingNormalChar evenMore)
            else runStreamingParser streamingNormalChar more
        Just (c, rest) ->
          if c /= '"' && c /= '\\' && c >= ' '
            then StreamDone rest c
            else StreamFailed $ "Invalid character in string: " ++ show c

streamingEscapedChar :: StreamingParser Char
streamingEscapedChar = do
  _ <- streamingChar '\\'
  c <- streamingAnyChar
  case c of
    '"' -> return '"'
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> streamingUnicodeEscape
    _ -> fail $ "Invalid escape sequence: \\" ++ [c]

streamingUnicodeEscape :: StreamingParser Char
streamingUnicodeEscape = do
  d1 <- streamingHexDigit
  d2 <- streamingHexDigit
  d3 <- streamingHexDigit
  d4 <- streamingHexDigit
  let hexStr = [d1, d2, d3, d4]
  case readHex hexStr of
    [(n, "")] -> return (chr n)
    _ -> fail "Invalid unicode escape"

streamingHexDigit :: StreamingParser Char
streamingHexDigit = StreamingParser $ \input ->
  case uncons input of
    Nothing -> StreamPartial $ \more ->
      if null more
        then StreamPartial (\evenMore -> runStreamingParser streamingHexDigit evenMore)
        else runStreamingParser streamingHexDigit more
    Just (c, rest) ->
      if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        then StreamDone rest c
        else StreamFailed $ "Expected hex digit but got " ++ show c

streamingJsonArray :: StreamingParser JsonValue
streamingJsonArray = do
  _ <- streamingChar '['
  streamingWhitespace
  values <- streamingSepBy streamingJsonValue (streamingWhitespace *> streamingChar ',' <* streamingWhitespace)
  streamingWhitespace
  _ <- streamingChar ']'
  return (JsonArray values)

streamingJsonObject :: StreamingParser JsonValue
streamingJsonObject = do
  _ <- streamingChar '{'
  streamingWhitespace
  pairs <- streamingSepBy streamingKeyValuePair (streamingWhitespace *> streamingChar ',' <* streamingWhitespace)
  streamingWhitespace
  _ <- streamingChar '}'
  return (JsonObject $ fromList pairs)

streamingKeyValuePair :: StreamingParser (Text, JsonValue)
streamingKeyValuePair = do
  key <- pack <$> streamingJsonStringContent
  streamingWhitespace
  _ <- streamingChar ':'
  streamingWhitespace
  value <- streamingJsonValue
  return (key, value)

streamingSepBy :: StreamingParser a -> StreamingParser sep -> StreamingParser [a]
streamingSepBy p sep = streamingSepBy1 p sep <|> pure []

streamingSepBy1 :: StreamingParser a -> StreamingParser sep -> StreamingParser [a]
streamingSepBy1 p sep = do
  first <- p
  rest <- streamingMany (sep *> p)
  return (first : rest)

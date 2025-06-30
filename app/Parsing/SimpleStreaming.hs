{-# LANGUAGE OverloadedStrings #-}

module Parsing.SimpleStreaming (
  SimpleStreamResult(..),
  simpleParseChar,
  simpleParseString,
  simpleParseJSON,
  feedInput
) where

import Data.Text.Lazy (Text, uncons, pack)
import qualified Data.Text.Lazy as TL
import Parsing.JSONParser (JsonValue(..), prettyPrintJson)

-- | Simple streaming result with explicit state preservation
data SimpleStreamResult a 
  = SimpleDone Text a                    -- ^ Parsing complete
  | SimpleNeedMore (Text -> SimpleStreamResult a)  -- ^ Need more input
  | SimpleFailed String                  -- ^ Parsing failed

instance (Show a) => Show (SimpleStreamResult a) where
  show (SimpleDone remaining result) = "SimpleDone " ++ show (TL.unpack remaining) ++ " " ++ show result
  show (SimpleNeedMore _) = "SimpleNeedMore <continuation>"
  show (SimpleFailed err) = "SimpleFailed " ++ show err

-- | Parse a single specific character
simpleParseChar :: Char -> Text -> SimpleStreamResult Char
simpleParseChar expected input =
  case uncons input of
    Nothing -> SimpleNeedMore (\more -> simpleParseChar expected more)
    Just (c, rest) -> 
      if c == expected 
        then SimpleDone rest c
        else SimpleFailed $ "Expected " ++ show expected ++ " but got " ++ show c

-- | Parse a specific string
simpleParseString :: String -> Text -> SimpleStreamResult String
simpleParseString target input = parseStringGo target input ""
  where
    parseStringGo [] inp acc = SimpleDone inp (reverse acc)
    parseStringGo (c:cs) inp acc =
      case uncons inp of
        Nothing -> SimpleNeedMore (\more -> parseStringGo (c:cs) more acc)
        Just (ch, rest) ->
          if ch == c
            then parseStringGo cs rest (c:acc)
            else SimpleFailed $ "Expected " ++ show c ++ " but got " ++ show ch

-- | Parse a simple JSON number (just digits for demonstration)
simpleParseNumber :: Text -> SimpleStreamResult JsonValue
simpleParseNumber input = 
  let (digits, rest) = TL.span (\c -> c >= '0' && c <= '9' || c == '.') input
  in if TL.null digits
     then SimpleNeedMore (\more -> simpleParseNumber (input `TL.append` more))
     else case reads (TL.unpack digits) of
            [(n, "")] -> SimpleDone rest (JsonNumber n)
            _ -> SimpleFailed "Invalid number"

-- | Parse simple JSON (just strings and numbers for demo)
simpleParseJSON :: Text -> SimpleStreamResult JsonValue
simpleParseJSON input =
  case TL.uncons (TL.dropWhile (`elem` (" \t\n\r" :: String)) input) of
    Nothing -> SimpleNeedMore (\more -> simpleParseJSON more)
    Just ('"', rest) -> 
      -- Parse string content until closing quote
      parseStringContent rest ""
    Just (c, _) | c >= '0' && c <= '9' -> 
      simpleParseNumber input
    Just (c, _) -> SimpleFailed $ "Unexpected character: " ++ show c

-- | Parse string content (simplified - no escape sequences for demo)
parseStringContent :: Text -> String -> SimpleStreamResult JsonValue
parseStringContent input acc =
  case uncons input of
    Nothing -> SimpleNeedMore (\more -> parseStringContent more acc)
    Just ('"', rest) -> SimpleDone rest (JsonString $ pack acc)
    Just (c, rest) -> parseStringContent rest (acc ++ [c])

-- | Feed more input to a continuation
feedInput :: Text -> SimpleStreamResult a -> SimpleStreamResult a
feedInput more (SimpleNeedMore cont) = cont more
feedInput _ result = result  -- Already complete or failed
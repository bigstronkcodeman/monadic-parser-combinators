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

data SimpleStreamResult a 
  = SimpleDone Text a                    
  | SimpleNeedMore (Text -> SimpleStreamResult a)  
  | SimpleFailed String                  

instance (Show a) => Show (SimpleStreamResult a) where
  show (SimpleDone remaining result) = "SimpleDone " ++ show (TL.unpack remaining) ++ " " ++ show result
  show (SimpleNeedMore _) = "SimpleNeedMore <continuation>"
  show (SimpleFailed err) = "SimpleFailed " ++ show err

simpleParseChar :: Char -> Text -> SimpleStreamResult Char
simpleParseChar expected input =
  case uncons input of
    Nothing -> SimpleNeedMore (\more -> simpleParseChar expected more)
    Just (c, rest) -> 
      if c == expected 
        then SimpleDone rest c
        else SimpleFailed $ "Expected " ++ show expected ++ " but got " ++ show c

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

simpleParseNumber :: Text -> SimpleStreamResult JsonValue
simpleParseNumber input = 
  let (digits, rest) = TL.span (\c -> c >= '0' && c <= '9' || c == '.') input
  in if TL.null digits
     then SimpleNeedMore (\more -> simpleParseNumber (input `TL.append` more))
     else case reads (TL.unpack digits) of
            [(n, "")] -> SimpleDone rest (JsonNumber n)
            _ -> SimpleFailed "Invalid number"

simpleParseJSON :: Text -> SimpleStreamResult JsonValue
simpleParseJSON input =
  case TL.uncons (TL.dropWhile (`elem` (" \t\n\r" :: String)) input) of
    Nothing -> SimpleNeedMore (\more -> simpleParseJSON more)
    Just ('"', rest) -> 
      parseStringContent rest ""
    Just (c, _) | c >= '0' && c <= '9' -> 
      simpleParseNumber input
    Just (c, _) -> SimpleFailed $ "Unexpected character: " ++ show c

parseStringContent :: Text -> String -> SimpleStreamResult JsonValue
parseStringContent input acc =
  case uncons input of
    Nothing -> SimpleNeedMore (\more -> parseStringContent more acc)
    Just ('"', rest) -> SimpleDone rest (JsonString $ pack acc)
    Just (c, rest) -> parseStringContent rest (acc ++ [c])

feedInput :: Text -> SimpleStreamResult a -> SimpleStreamResult a
feedInput more (SimpleNeedMore cont) = cont more
feedInput _ result = result  

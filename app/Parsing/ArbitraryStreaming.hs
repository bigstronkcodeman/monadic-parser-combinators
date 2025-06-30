{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Parsing.ArbitraryStreaming (
  StreamingState(..),
  StreamingResult(..),
  initState,
  feedChunk,
  processStream
) where

import qualified Data.Text.Lazy as TL
import Data.Char (isDigit, isSpace)
import Control.Monad.State
import Parsing.JSONParser (JsonValue(..))
import qualified Data.HashMap.Lazy as HM

data StreamingState = StreamingState {
  buffer :: !TL.Text,           
  stack :: ![StackFrame],       
  complete :: ![JsonValue]      
} deriving (Show)

data StackFrame 
  = ObjectFrame !(HM.HashMap TL.Text JsonValue) !(Maybe TL.Text) 
  | ArrayFrame ![JsonValue]                                      
  deriving (Show)

data StreamingResult = StreamingResult {
  newState :: !StreamingState,
  values :: ![JsonValue],       
  needMore :: !Bool             
} deriving (Show)

initState :: StreamingState
initState = StreamingState TL.empty [] []

feedChunk :: TL.Text -> StreamingState -> StreamingResult
feedChunk chunk state = 
  let newBuffer = buffer state `TL.append` chunk
      state' = state { buffer = newBuffer }
  in processBuffer state'

processBuffer :: StreamingState -> StreamingResult
processBuffer state = 
  let (values, finalState) = runState (parseLoop []) state
  in StreamingResult finalState values (needMoreInput finalState)
  where
    needMoreInput st = not (TL.null (buffer st))
    
    parseLoop :: [JsonValue] -> State StreamingState [JsonValue]
    parseLoop acc = do
      st <- get
      case parseNext (buffer st) of
        ParseComplete value remaining -> do
          put st { buffer = remaining }
          parseLoop (value : acc)
        ParseIncomplete newSt -> do
          put newSt
          return (reverse acc)
        ParseError err -> do
          return (reverse acc)

data ParseResult 
  = ParseComplete JsonValue TL.Text  
  | ParseIncomplete StreamingState   
  | ParseError String                

parseNext :: TL.Text -> ParseResult
parseNext input = 
  case parseValue (skipWhitespace input) of
    Just (value, remaining) -> ParseComplete value remaining
    Nothing -> ParseIncomplete (StreamingState input [] [])

parseValue :: TL.Text -> Maybe (JsonValue, TL.Text)
parseValue input
  | TL.null input = Nothing
  | otherwise = case TL.head input of
      '"' -> parseString input
      '{' -> parseObject input
      '[' -> parseArray input
      't' -> parseKeyword "true" (JsonBool True) input
      'f' -> parseKeyword "false" (JsonBool False) input
      'n' -> parseKeyword "null" JsonNull input
      c | isDigit c || c == '-' -> parseNumber input
      _ -> Nothing

parseString :: TL.Text -> Maybe (JsonValue, TL.Text)
parseString input
  | TL.null input || TL.head input /= '"' = Nothing
  | otherwise = parseStringContent (TL.tail input) ""
  where
    parseStringContent :: TL.Text -> String -> Maybe (JsonValue, TL.Text)
    parseStringContent inp acc
      | TL.null inp = Nothing  
      | otherwise = case TL.head inp of
          '"' -> Just (JsonString (TL.pack acc), TL.tail inp)
          '\\' -> parseEscape (TL.tail inp) acc
          c -> parseStringContent (TL.tail inp) (acc ++ [c])
    
    parseEscape :: TL.Text -> String -> Maybe (JsonValue, TL.Text)
    parseEscape inp acc
      | TL.null inp = Nothing  
      | otherwise = case TL.head inp of
          '"' -> parseStringContent (TL.tail inp) (acc ++ "\"")
          '\\' -> parseStringContent (TL.tail inp) (acc ++ "\\")
          'n' -> parseStringContent (TL.tail inp) (acc ++ "\n")
          't' -> parseStringContent (TL.tail inp) (acc ++ "\t")
          'r' -> parseStringContent (TL.tail inp) (acc ++ "\r")
          'b' -> parseStringContent (TL.tail inp) (acc ++ "\b")
          'f' -> parseStringContent (TL.tail inp) (acc ++ "\f")
          '/' -> parseStringContent (TL.tail inp) (acc ++ "/")
          'u' -> parseUnicodeEscape (TL.tail inp) acc
          _ -> Nothing
    
    parseUnicodeEscape :: TL.Text -> String -> Maybe (JsonValue, TL.Text)
    parseUnicodeEscape inp acc
      | TL.length inp < 4 = Nothing  
      | otherwise = 
          let hexChars = TL.take 4 inp
              rest = TL.drop 4 inp
          in case parseHex (TL.unpack hexChars) of
               Just codePoint -> parseStringContent rest (acc ++ [toEnum codePoint])
               Nothing -> Nothing
    
    parseHex :: String -> Maybe Int
    parseHex str = case reads ("0x" ++ str) of
      [(n, "")] -> Just n
      _ -> Nothing

parseNumber :: TL.Text -> Maybe (JsonValue, TL.Text)
parseNumber input = parseFloatLike input
  where
    parseFloatLike :: TL.Text -> Maybe (JsonValue, TL.Text)
    parseFloatLike inp
      | TL.null inp = Nothing
      | otherwise = 
          let (sign, afterSign) = parseSign inp
              (wholePart, afterWhole) = parseDigits afterSign
              (hasDot, fractionalPart, afterFrac) = parseFractional afterWhole
              (hasExp, expValue, remaining) = parseExponent afterFrac
              
              wholeValue = maybe 0 id wholePart
              fracValue = calculateFractional fractionalPart
              baseValue = fromIntegral wholeValue + fracValue
              finalValue = if hasExp 
                          then baseValue * (10 ^^ expValue)
                          else baseValue
              result = sign * finalValue
              
          in if wholePart == Nothing && not hasDot
             then Nothing  
             else Just (JsonNumber result, remaining)
    
    parseSign :: TL.Text -> (Double, TL.Text)
    parseSign inp
      | TL.null inp = (1, inp)
      | TL.head inp == '-' = (-1, TL.tail inp)
      | TL.head inp == '+' = (1, TL.tail inp)  
      | otherwise = (1, inp)
    
    parseDigits :: TL.Text -> (Maybe Int, TL.Text)
    parseDigits inp =
      let (digitStr, remaining) = TL.span isDigit inp
      in if TL.null digitStr
         then (Nothing, inp)
         else case reads (TL.unpack digitStr) of
                [(n, "")] -> (Just n, remaining)
                _ -> (Nothing, inp)
    
    parseFractional :: TL.Text -> (Bool, TL.Text, TL.Text)
    parseFractional inp
      | TL.null inp || TL.head inp /= '.' = (False, TL.empty, inp)
      | otherwise = 
          let afterDot = TL.tail inp
              (fracDigits, remaining) = TL.span isDigit afterDot
          in (True, fracDigits, remaining)
    
    calculateFractional :: TL.Text -> Double
    calculateFractional fracDigits
      | TL.null fracDigits = 0
      | otherwise = 
          let digits = TL.unpack fracDigits
              len = length digits
          in case reads digits of
               [(n, "")] -> fromIntegral n * (10 ^^ negate len)
               _ -> 0
    
    parseExponent :: TL.Text -> (Bool, Int, TL.Text)
    parseExponent inp
      | TL.null inp = (False, 0, inp)
      | TL.head inp `elem` ("eE" :: String) = 
          let afterE = TL.tail inp
              (expSign, afterExpSign) = parseExpSign afterE
              (expDigits, remaining) = TL.span isDigit afterExpSign
          in if TL.null expDigits
             then (False, 0, inp)  
             else case reads (TL.unpack expDigits) of
                    [(n, "")] -> (True, expSign * n, remaining)
                    _ -> (False, 0, inp)
      | otherwise = (False, 0, inp)
    
    parseExpSign :: TL.Text -> (Int, TL.Text)
    parseExpSign inp
      | TL.null inp = (1, inp)
      | TL.head inp == '-' = (-1, TL.tail inp)
      | TL.head inp == '+' = (1, TL.tail inp)
      | otherwise = (1, inp)

parseObject :: TL.Text -> Maybe (JsonValue, TL.Text)
parseObject input
  | TL.null input || TL.head input /= '{' = Nothing
  | otherwise = parseObjectContent (skipWhitespace (TL.tail input)) HM.empty
  where
    parseObjectContent :: TL.Text -> HM.HashMap TL.Text JsonValue -> Maybe (JsonValue, TL.Text)
    parseObjectContent inp obj
      | TL.null inp = Nothing  
      | TL.head inp == '}' = Just (JsonObject obj, TL.tail inp)
      | HM.null obj = parseFirstPair inp obj  
      | TL.head inp == ',' = parseNextPair (skipWhitespace (TL.tail inp)) obj
      | otherwise = Nothing
    
    parseFirstPair :: TL.Text -> HM.HashMap TL.Text JsonValue -> Maybe (JsonValue, TL.Text)
    parseFirstPair inp obj = case parseKeyValuePair inp of
      Just ((key, value), remaining) -> 
        parseObjectContent (skipWhitespace remaining) (HM.insert key value obj)
      Nothing -> Nothing
    
    parseNextPair :: TL.Text -> HM.HashMap TL.Text JsonValue -> Maybe (JsonValue, TL.Text)
    parseNextPair inp obj = case parseKeyValuePair inp of
      Just ((key, value), remaining) -> 
        parseObjectContent (skipWhitespace remaining) (HM.insert key value obj)
      Nothing -> Nothing

parseKeyValuePair :: TL.Text -> Maybe ((TL.Text, JsonValue), TL.Text)
parseKeyValuePair input = do
  (JsonString key, afterKey) <- parseString input
  let afterColon = skipWhitespace afterKey
  if TL.null afterColon || TL.head afterColon /= ':'
    then Nothing
    else do
      (value, remaining) <- parseValue (skipWhitespace (TL.tail afterColon))
      return ((key, value), remaining)

parseArray :: TL.Text -> Maybe (JsonValue, TL.Text)
parseArray input
  | TL.null input || TL.head input /= '[' = Nothing
  | otherwise = parseArrayContent (skipWhitespace (TL.tail input)) []
  where
    parseArrayContent :: TL.Text -> [JsonValue] -> Maybe (JsonValue, TL.Text)
    parseArrayContent inp elements
      | TL.null inp = Nothing  
      | TL.head inp == ']' = Just (JsonArray (reverse elements), TL.tail inp)
      | null elements = parseFirstElement inp elements  
      | TL.head inp == ',' = parseNextElement (skipWhitespace (TL.tail inp)) elements
      | otherwise = Nothing
    
    parseFirstElement :: TL.Text -> [JsonValue] -> Maybe (JsonValue, TL.Text)
    parseFirstElement inp elements = case parseValue inp of
      Just (value, remaining) -> 
        parseArrayContent (skipWhitespace remaining) (value : elements)
      Nothing -> Nothing
    
    parseNextElement :: TL.Text -> [JsonValue] -> Maybe (JsonValue, TL.Text)
    parseNextElement inp elements = case parseValue inp of
      Just (value, remaining) -> 
        parseArrayContent (skipWhitespace remaining) (value : elements)
      Nothing -> Nothing

parseKeyword :: TL.Text -> JsonValue -> TL.Text -> Maybe (JsonValue, TL.Text)
parseKeyword keyword value input
  | TL.take (TL.length keyword) input == keyword = 
      Just (value, TL.drop (TL.length keyword) input)
  | otherwise = Nothing

skipWhitespace :: TL.Text -> TL.Text
skipWhitespace = TL.dropWhile isSpace

processStream :: (JsonValue -> IO Bool) -> [TL.Text] -> IO Int
processStream processor chunks = go initState chunks 0
  where
    go :: StreamingState -> [TL.Text] -> Int -> IO Int
    go _ [] count = return count
    go state (chunk:rest) count = do
      let result = feedChunk chunk state
      newCount <- processValues (values result) processor count
      if needMore result
        then go (newState result) rest newCount
        else return newCount
    
    processValues :: [JsonValue] -> (JsonValue -> IO Bool) -> Int -> IO Int
    processValues [] _ count = return count
    processValues (v:vs) proc count = do
      shouldContinue <- proc v
      if shouldContinue
        then processValues vs proc (count + 1)
        else return (count + 1)

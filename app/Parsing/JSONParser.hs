{-# LANGUAGE OverloadedStrings #-}

module Parsing.JSONParser (
  JsonValue (..),
  jsonValue
) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.HashMap.Lazy (HashMap, fromList)
import Data.Text.Lazy (Text)
import Parsing.Combinators (sepBy)
import Parsing.ParserTypes (Parser)
import Parsing.Primitives (takeWhile, (<?>))
import Parsing.UtilParsers (char, float, text)
import Prelude hiding (takeWhile)

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
      content <- takeWhile (/= '"')
      _ <- char '"'
      return content
  )
    <?> "jsonKey"

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

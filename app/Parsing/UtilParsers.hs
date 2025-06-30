{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Parsing.UtilParsers (
  satisfyWithErrorMsg,
  satisfy,
  char,
  digit,
  space,
  text,
  token,
  symbol,
  bool,
  int,
  float
) where

import Parsing.ParserTypes (Parser(..))
import Parsing.Primitives (item, (<?>), peek, takeWhile, takeWhile1, takeWhile1WithErrorMsg)
import Parsing.Combinators (optional)
import Data.Char (isDigit, digitToInt, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy as T (Text, uncons, empty, elem, foldl', length)
import Text.Printf (printf)
import Control.Applicative (Alternative(..))
import Control.Monad (when, void)
import Prelude hiding (takeWhile)

satisfyWithErrorMsg :: (Char -> Bool) -> (Char -> String) -> Parser Char
satisfyWithErrorMsg p km = do
  c <- peek item
  if p c then item
  else fail (km c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = satisfyWithErrorMsg p (const "unsatisfied predicate")

char :: Char -> Parser Char
char c = (satisfyWithErrorMsg (== c) (printf "expected '%c' but got '%c'" c) <?> printf "satisfy (== '%c')" c) <?> printf "char '%c'" c

digit :: Parser Char
digit = (satisfyWithErrorMsg isDigit (printf "expected a digit but got '%c'") <?> "satisfy isDigit") <?> "digit"

space :: Parser Char
space = (satisfyWithErrorMsg (`T.elem` "\t ") (printf "expected a space but got '%c'") <?> "satisfy (`elem` \"\\t \")") <?> "space"

text :: Text -> Parser Text
text t = (do
  case uncons t of
    Nothing -> return T.empty
    Just (c, cs) -> do
      _ <- char c
      _ <- text cs
      return t)
  <?> printf "text \"%v\"" t

token :: Parser a -> Parser a
token p = (many space >> p >>= \v -> many space >> return v) <?> "token"

symbol :: Text -> Parser Text
symbol t = token (text t) <?> printf "symbol \"%v\"" t

nextSymbol :: Parser Text
nextSymbol = token (takeWhile1WithErrorMsg (not . (`T.elem` "\t ")) $ printf "expected non-whitespace but got '%c'") <?> "nextSymbol"

bool :: Parser Bool
bool = (== "True") <$> (text "True" <|> text "False")

natAccumulator :: (Num a) => a -> Char -> a
natAccumulator z d = 10 * z + fromIntegral (digitToInt d)

pureDigsInt :: (Integral a) => Parser a
pureDigsInt = (T.foldl' natAccumulator 0 <$> takeWhile1 isDigit) <?> "pureDigsInt"

int :: (Integral a) => Parser a
int = (do
  signMb <- optional $ token (char '-') <|> token (char '+')
  case signMb of
    Nothing  -> pureDigsInt
    Just '-' -> fmap negate pureDigsInt
    Just '+' -> pureDigsInt
    _        -> undefined)
  <?> "int"

float :: (RealFrac a) => Parser a
float = (do
  sign <- peek item
  let !finalSign = if sign == '-' then -1 else 1
  when (sign == '+' || sign == '-') $
    void (item <* takeWhile (\c -> c /= '\n' && isSpace c))
  wholePartMb <- optional (pureDigsInt :: Parser Int)
  pointMb <- case wholePartMb of
    Nothing -> pure <$> char '.'
    _       -> optional (char '.')
  floatDigsMb <- case pointMb of
    Nothing -> pure Nothing
    _       -> case wholePartMb of
      Nothing -> pure <$> takeWhile1 isDigit
      _       -> optional (takeWhile isDigit)
  let !floatPart 
        | Just digs <- floatDigsMb = 
            let pow10 = negate (T.length digs)
            in T.foldl' natAccumulator 0 digs * 10 ^^ pow10
        | otherwise = 0
  eMb <- peek (optional item)
  let !preExpResult = finalSign * (fromIntegral (fromMaybe 0 wholePartMb) + floatPart)
  case eMb of
    Nothing -> return preExpResult
    Just c 
      | c == 'e' || c == 'E' -> (do
        _ <- item
        esign <- peek item
        let !eNegative = esign == '-'
        when (esign == '+' || esign =='-') $
          void item
        e <- pureDigsInt :: Parser Int
        return (if eNegative
          then preExpResult * 10 ^^ negate e
          else preExpResult * 10 ^^ e)) <|> return preExpResult
      | otherwise            -> return preExpResult)
  <?> "float"

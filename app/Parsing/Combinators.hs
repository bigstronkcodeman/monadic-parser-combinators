{-# LANGUAGE BangPatterns #-}

module Parsing.Combinators (
  sepBy,
  sepBy1,
  optional,
  (<#>),
  default_
) where

import Parsing.ParserTypes (Parser(..))
import Parsing.Primitives ((<?>))
import Control.Applicative (Alternative(..))
import Prelude hiding (fail)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p d = (do
  xs <- many (p >>= \x -> d >> return x)
  xMb <- optional p
  case xMb of
    Nothing -> return xs
    Just x -> return (xs ++ [x]))
  <?> "sepBy"

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p d = (do
  x <- p
  xs <- many (d >> p)
  return (x:xs))
  <?> "sepBy 1"

optional :: Parser a -> Parser (Maybe a)
optional p = Parser (\inp !pos _ ks ->
  let kf' _ _ _ _     = ks inp pos Nothing
      ks' inp' pos' x = ks inp' pos' (Just x)
  in runParser p inp pos kf' ks')
  <?> "optional"

default_ :: a -> Parser a -> Parser a
default_ x p = (do
  resMb <- optional p
  case resMb of
    Nothing -> return x
    Just res -> return res)
  <?> "default_"

(<#>) :: Parser a -> Parser a -> Parser a
p <#> q = Parser (\inp !pos kf ks ->
  let kfp inp' !pos' _ _ = runParser q inp' pos' kf ks
  in runParser p inp pos kfp ks)
  <?> "<#>"

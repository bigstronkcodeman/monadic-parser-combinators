{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parsing.Primitives (
  parse,
  (<?>),
  item,
  peek,
  peekInput,
  takeWhile,
  takeWhile1,
  takeWhile1WithErrorMsg
) where

import Parsing.ParserTypes (Parser(..), Result(..), Pos(..))
import Data.Text.Lazy as T (Text, uncons, cons, span, foldl')
import Prelude hiding (fail, takeWhile)

(<?>) :: Parser a -> String -> Parser a
p <?> s = Parser $ \inp !pos kf ks ->
  let kf' inp' !pos' ctx = kf inp' pos' (s:ctx)
  in runParser p inp pos kf' ks

parse :: Parser a -> Text -> Result a
parse p t = let kf inp !pos = Failure inp pos
                ks inp _    = Finished inp
            in runParser p t (Pos 0 0) kf ks

peek :: Parser a -> Parser a
peek p = Parser (\inp !pos kf ks ->
  let ks' _ _ = ks inp pos
  in runParser p inp pos kf ks')
  <?> "peek"

peekInput :: Parser Text
peekInput = Parser (\inp !pos _ ks ->
  ks inp pos inp)
  <?> "peekInput"

retroUpdatePos :: Text -> Pos -> Pos
retroUpdatePos txt pos = T.foldl' (flip updatePos) pos txt

updatePos :: Char -> Pos -> Pos
updatePos '\n' (Pos row _)   = Pos (row+1) 0
updatePos _    (Pos row col) = Pos row (col+1)

item :: Parser Char
item = Parser (\inp !pos kf ks ->
  case uncons inp of
    Nothing -> kf inp pos [] "unexepected EOF"
    Just (c, cs) -> ks cs (updatePos c pos) c)
  <?> "item"

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = Parser (\inp !pos _ ks ->
  let (result, rest) = T.span p inp
  in ks rest (retroUpdatePos result pos) result)
  <?> "takeWhile"

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = takeWhile1WithErrorMsg p $ const "unsatisfied predicate"

takeWhile1WithErrorMsg :: (Char -> Bool) -> (Char -> String) -> Parser Text
takeWhile1WithErrorMsg p km = Parser (\inp !pos kf ks ->
  case T.uncons inp of
    Nothing -> kf inp pos [] "unexpected EOF"
    Just (c, cs) ->
      if p c then
        let (resultTail, rest) = T.span p cs
        in ks rest (retroUpdatePos resultTail (updatePos c pos)) $ T.cons c resultTail
      else
        kf inp pos [] $ km c)
  <?> "takeWhile1WithErrorMsg"

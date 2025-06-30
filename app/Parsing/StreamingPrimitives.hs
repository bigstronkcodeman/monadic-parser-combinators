{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.StreamingPrimitives (
  streamingItem,
  streamingTakeWhile,
  parseStreaming
) where

import Parsing.ParserTypes (Parser(..), Result(..), Pos(..))
import Data.Text.Lazy as T (Text, uncons, cons, span, empty, null, foldl')
import Prelude hiding (fail, takeWhile)

-- | Parse that continues with more input instead of failing on EOF
parseStreaming :: Parser a -> Text -> Result a
parseStreaming p t = 
  let kf inp !pos ctx err = 
        if T.null inp && Prelude.null err  -- Only partial if we actually need more input
          then Partial (\moreInput -> 
            -- Continue parsing with the additional input
            runParser p moreInput pos 
              (\inp' pos' ctx' err' -> 
                if T.null inp' 
                  then Partial (\evenMore -> parseStreaming p (inp' `mappend` evenMore))
                  else Failure inp' pos' ctx' err')
              (\inp' pos' result -> Finished inp' result))
          else Failure inp pos ctx err
      ks inp !pos result = Finished inp result
  in runParser p t (Pos 0 0) kf ks

-- | Item parser that returns Partial when it needs more input
streamingItem :: Parser Char
streamingItem = Parser (\inp !pos kf ks ->
  case uncons inp of
    Nothing -> 
      -- Instead of failing, return Partial with a continuation
      Partial (\moreInput -> 
        if T.null moreInput 
          then Partial (\evenMore -> runParser streamingItem evenMore pos kf ks)
          else runParser streamingItem moreInput pos kf ks)
    Just (c, cs) -> ks cs (updatePos c pos) c)
  where
    updatePos '\n' (Pos row _)   = Pos (row+1) 0
    updatePos _    (Pos row col) = Pos row (col+1)

-- | TakeWhile that can resume when it needs more input
streamingTakeWhile :: (Char -> Bool) -> Parser Text
streamingTakeWhile p = Parser (\inp !pos kf ks ->
  let (result, rest) = T.span p inp
  in if T.null rest && not (T.null inp)
     then -- We consumed all input but might need more
       Partial (\moreInput ->
         let combinedInput = rest `mappend` moreInput
             (moreResult, finalRest) = T.span p combinedInput
             totalResult = result `mappend` moreResult
             newPos = retroUpdatePos totalResult pos
         in ks finalRest newPos totalResult)
     else
       let newPos = retroUpdatePos result pos
       in ks rest newPos result)
  where
    retroUpdatePos txt pos = T.foldl' (flip updatePos) pos txt
    updatePos '\n' (Pos row _)   = Pos (row+1) 0
    updatePos _    (Pos row col) = Pos row (col+1)
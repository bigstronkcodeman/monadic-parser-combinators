{-# LANGUAGE OverloadedStrings #-}

module Parsing.StreamingArrayParser (
  processJsonArrayFile
) where

import qualified Data.Text.Lazy.IO as T
import System.IO (Handle)
import Text.Printf (printf)
import Parsing.JSONParser (JsonValue(..))
import Parsing.Primitives (parse)
import Parsing.ParserTypes (Result(..))
import qualified Parsing.JSONParser as JP

processJsonArrayFile :: Handle -> (JsonValue -> IO Bool) -> IO Int
processJsonArrayFile handle processor = do
  content <- T.hGetContents handle
  case parse JP.jsonValue content of
    Finished _ (JsonArray elements) -> do
      printf "Found JSON array with %d elements\n" (length elements)
      processElements elements processor 0
    Finished _ _ -> do
      printf "File does not contain a JSON array\n"
      return 0
    Failure _ _ _ err -> do
      printf "Failed to parse JSON: %s\n" err
      return 0
    Partial _ -> do
      printf "Incomplete JSON file\n"
      return 0
  where
    processElements :: [JsonValue] -> (JsonValue -> IO Bool) -> Int -> IO Int
    processElements [] _ count = return count
    processElements (element:rest) proc count = do
      printf "Processing element %d...\n" (count + 1)
      shouldContinue <- proc element
      if shouldContinue
        then processElements rest proc (count + 1)
        else do
          printf "Processing stopped by user callback at element %d\n" (count + 1)
          return (count + 1)

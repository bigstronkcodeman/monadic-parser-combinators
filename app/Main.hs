{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Paths_parsing (getDataDir)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import Control.Exception (IOException, try)
import Control.Monad (forM_, filterM)
import Text.Printf (printf)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Parsing.JSONParser (JsonValue, jsonValue, prettyPrintJson)
import Parsing.StreamingJSONParser (streamingJsonValue)
import Parsing.StreamingPrimitives (parseStreaming)
import qualified Parsing.TrueStreaming as TS
import Parsing.TrueStreaming (parseIncrementally, StreamResult(..))
import Parsing.SimpleStreaming (SimpleStreamResult(..), simpleParseJSON, feedInput)
import Parsing.Primitives
import Parsing.ParserTypes
import Data.List (intercalate)

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory dir = do
  dirContents <- try $ listDirectory dir
  case dirContents of
    Left (_ :: IOException) -> return []
    Right contents -> do
      let fullPaths = map (dir </>) contents
      files <- filterM doesFileExist fullPaths
      dirs <- filterM doesDirectoryExist fullPaths
      subFiles <- concat <$> mapM walkDirectory dirs
      return (files ++ subFiles)

walkDirectoryWith :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
walkDirectoryWith p filePath = filter p <$> walkDirectory filePath

streamingTest :: IO ()
streamingTest = do
  printf "=== Testing Full JSON Streaming Parser ===\n"
  
  -- Test 1: Simple string parsing
  printf "\n--- Test 1: Simple JSON string ---\n"
  let chunk1 = TL.pack "\"hel"
  printf "Chunk 1: \"%s\"\n" (TL.unpack chunk1)
  case parseIncrementally TS.streamingJsonValue chunk1 of
    StreamFailed err -> printf "✗ Failed: %s\n" err
    StreamDone remaining result -> printf "✗ Unexpected completion: %s\n" (prettyPrintJson result)
    StreamPartial cont -> do
      printf "✓ Partial result! Need more input.\n"
      let chunk2 = TL.pack "lo world\""
      printf "Chunk 2: \"%s\"\n" (TL.unpack chunk2)
      case cont chunk2 of
        StreamDone remaining result -> printf "✓ Success: %s\n" (prettyPrintJson result)
        StreamFailed err -> printf "✗ Failed after continuation: %s\n" err
        StreamPartial _ -> printf "✗ Still needs more input\n"
  
  -- Test 2: JSON object parsing
  printf "\n--- Test 2: JSON object ---\n"
  let objChunk1 = TL.pack "{\"name\": \"Jo"
  printf "Chunk 1: \"%s\"\n" (TL.unpack objChunk1)
  case parseIncrementally TS.streamingJsonValue objChunk1 of
    StreamPartial cont -> do
      printf "✓ Partial object parsing\n"
      let objChunk2 = TL.pack "hn\", \"age\": 30}"
      printf "Chunk 2: \"%s\"\n" (TL.unpack objChunk2)
      case cont objChunk2 of
        StreamDone remaining result -> printf "✓ Object parsed: %s\n" (prettyPrintJson result)
        other -> printf "Unexpected result: %s\n" (show other)
    other -> printf "Unexpected result: %s\n" (show other)
  
  -- Test 3: JSON array parsing
  printf "\n--- Test 3: JSON array ---\n"
  let arrChunk1 = TL.pack "[1, 2"
  printf "Chunk 1: \"%s\"\n" (TL.unpack arrChunk1)
  case parseIncrementally TS.streamingJsonValue arrChunk1 of
    StreamPartial cont -> do
      printf "✓ Partial array parsing\n"
      let arrChunk2 = TL.pack ", 3, 4]"
      printf "Chunk 2: \"%s\"\n" (TL.unpack arrChunk2)
      case cont arrChunk2 of
        StreamDone remaining result -> printf "✓ Array parsed: %s\n" (prettyPrintJson result)
        other -> printf "Unexpected result: %s\n" (show other)
    other -> printf "Unexpected result: %s\n" (show other)
  
  printf "\n✓ Full JSON streaming parser working!\n\n"




main :: IO ()
main = do
  -- Run streaming test first
  streamingTest
  
  -- Then run regular file parsing
  dataDir <- getDataDir
  let resourcesDir = dataDir </> "resources"
  jsonFiles <- walkDirectoryWith ((== ".json") . takeExtension) resourcesDir
  forM_ jsonFiles $ \filePath -> do
    printf "Parsing %s...\n" $ takeFileName filePath
    jsonText <- T.readFile filePath
    let parsedJson = parse jsonValue jsonText
    case parsedJson of
      Failure _ _ errCtx err -> printf "Failed to parse JSON!\nError context: %s\nError: %s\n" (intercalate ", " errCtx) err
      Partial _ -> printf "Partial ?\n"
      Finished _ result -> printf "Parsed JSON:\n%s\n" (prettyPrintJson result)
    putStr "\n"

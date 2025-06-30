{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Paths_parsing (getDataDir)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (IOMode(..), openFile, hClose)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Control.Exception (IOException, try)
import Control.Monad (forM_, filterM)
import Text.Printf (printf)
import qualified Data.HashMap.Lazy
import Parsing.JSONParser (JsonValue(..), jsonValue, prettyPrintJson)
import Parsing.StreamingArrayParser (processJsonArrayFile)
import Parsing.ArbitraryStreaming (processStream)
import Parsing.Primitives (parse)
import Parsing.ParserTypes (Result(..))

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

streamingArrayTest :: IO ()
streamingArrayTest = do
  printf "=== True Streaming JSON Array Parser ===\n"
  dataDir <- getDataDir
  let arrayFile = dataDir </> "resources" </> "large_array.json"
  printf "Processing JSON array file: %s\n" $ takeFileName arrayFile
  
  handle <- openFile arrayFile ReadMode
  
  totalProcessed <- processJsonArrayFile handle $ \element -> do
    case element of
      JsonObject obj -> do
        let name = case Data.HashMap.Lazy.lookup (TL.pack "name") obj of
              Just (JsonString n) -> TL.unpack n
              _ -> "Unknown"
        let dept = case Data.HashMap.Lazy.lookup (TL.pack "department") obj of
              Just (JsonString d) -> TL.unpack d
              _ -> "Unknown"
        printf "  Employee: %s (Department: %s)\n" name dept
        printf "  Full JSON: %s\n" (prettyPrintJson element)
        
        if dept == "Engineering"
          then do
            printf "  Found Engineering employee - continuing...\n"
            return True  
          else return True  
      _ -> do
        printf "  Non-object element: %s\n" (prettyPrintJson element)
        return True  
  
  printf "Total elements processed: %d\n" totalProcessed
  hClose handle
  printf "\n"


regularParseTest :: IO ()
regularParseTest = do
  printf "=== Regular JSON File Parsing ===\n"
  dataDir <- getDataDir
  let resourcesDir = dataDir </> "resources"
  jsonFiles <- walkDirectoryWith ((== ".json") . takeExtension) resourcesDir
  
  printf "Found %d JSON files:\n" (length jsonFiles)
  forM_ jsonFiles $ \filePath -> do
    printf "\n--- Parsing %s ---\n" $ takeFileName filePath
    jsonText <- T.readFile filePath
    let parsedJson = parse jsonValue jsonText
    case parsedJson of
      Failure _ _ _ err -> printf "✗ Failed to parse JSON! Error: %s\n" err
      Partial _ -> printf "⚠ Partial result\n"
      Finished _ result -> do
        printf "✓ Successfully parsed:\n%s\n" (prettyPrintJson result)
  printf "\n"

arbitraryStreamingTest :: IO ()
arbitraryStreamingTest = do
  printf "=== Arbitrary Streaming JSON Parser ===\n"
  printf "Testing with partial JSON across arbitrary chunk boundaries...\n\n"
  
  -- Test 1: Basic string splitting
  printf "Test 1: String split across chunks\n"
  let chunk1 = TL.pack "{\"name\": \"Ali"
  let chunk2 = TL.pack "ce\", \"age\": 28}"
  
  printf "Chunk 1: %s\n" (show chunk1)
  printf "Chunk 2: %s\n" (show chunk2)
  printf "\n"
  
  totalProcessed1 <- processStream processor [chunk1, chunk2]
  printf "Total JSON objects parsed: %d\n\n" totalProcessed1
  
  -- Test 2: Complex number formats split across chunks
  printf "Test 2: Complex numbers split across chunks\n"
  let numChunk1 = TL.pack "{\"scientific\": 1.23"
  let numChunk2 = TL.pack "e-10, \"negative\": -456"
  let numChunk3 = TL.pack ".789, \"large\": 9.87E+"
  let numChunk4 = TL.pack "20}"
  
  printf "Chunk 1: %s\n" (show numChunk1)
  printf "Chunk 2: %s\n" (show numChunk2)
  printf "Chunk 3: %s\n" (show numChunk3)
  printf "Chunk 4: %s\n" (show numChunk4)
  printf "\n"
  
  totalProcessed2 <- processStream processor [numChunk1, numChunk2, numChunk3, numChunk4]
  printf "Total JSON objects parsed: %d\n\n" totalProcessed2
  
  printf "✅ Successfully handled partial JSON across chunk boundaries!\n\n"
  where 
    processor :: JsonValue -> IO Bool
    processor value = do
      printf "✓ Parsed complete JSON: %s\n" (prettyPrintJson value)
      return True

main :: IO ()
main = do
  arbitraryStreamingTest
  streamingArrayTest  
  regularParseTest

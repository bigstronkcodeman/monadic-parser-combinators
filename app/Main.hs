{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Paths_parsing (getDataDir)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (IOMode(..), openFile, hClose, hSetBuffering, BufferMode(..), Handle)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
  printf "=== Streaming JSON Array Parser ===\n"
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
      Failure _ _ _ err -> printf "Failed to parse JSON! Error: %s\n" err
      Partial _ -> printf "Partial result\n"
      Finished _ result -> do
        printf "Successfully parsed:\n%s\n" (prettyPrintJson result)
  printf "\n"

arbitraryStreamingTest :: IO ()
arbitraryStreamingTest = do
  printf "=== Arbitrary Streaming JSON Parser ===\n"
  printf "Testing with real buffered file reading...\n\n"
  
  dataDir <- getDataDir
  let resourcesDir = dataDir </> "resources"
  
  printf "Test 1: complex_numbers.json with 32-byte buffer chunks\n"
  let complexFile = resourcesDir </> "complex_numbers.json"
  printf "File: %s\n" $ takeFileName complexFile
  
  handle1 <- openFile complexFile ReadMode
  totalProcessed1 <- readFileInChunks handle1 32 processor
  hClose handle1
  printf "Total JSON objects parsed: %d\n\n" totalProcessed1
  
  printf "Test 2: Reading simple_unicode.json with 16-byte buffer chunks\n"
  let unicodeFile = resourcesDir </> "json_examples" </> "simple_unicode.json"
  printf "File: %s\n" $ takeFileName unicodeFile
  
  handle2 <- openFile unicodeFile ReadMode
  totalProcessed2 <- readFileInChunks handle2 16 processor
  hClose handle2
  printf "Total JSON objects parsed: %d\n\n" totalProcessed2
  
  printf "✅ Successfully parsed files using real buffered streaming!\n\n"
  where 
    processor :: JsonValue -> IO Bool
    processor value = do
      printf "✓ Parsed complete JSON from disk chunks:\n%s\n" (prettyPrintJson value)
      return True
    
    readFileInChunks :: Handle -> Int -> (JsonValue -> IO Bool) -> IO Int
    readFileInChunks handle chunkSize proc = do
      chunks <- readAllChunks handle chunkSize []
      printf "Read %d chunks from file\n" (length chunks)
      forM_ (zip [1::Int ..] chunks) $ \(i, chunk) -> 
        printf "  Chunk %d: %d bytes\n" i (TL.length chunk)
      printf "\n"
      processStream proc chunks
    
    readAllChunks :: Handle -> Int -> [TL.Text] -> IO [TL.Text]
    readAllChunks handle chunkSize acc = do
      bs <- BS.hGetSome handle chunkSize
      if BS.null bs
        then return (reverse acc)
        else do
          let chunk = TLE.decodeUtf8 (BSL.fromStrict bs)
          readAllChunks handle chunkSize (chunk : acc)

main :: IO ()
main = do
  arbitraryStreamingTest
  streamingArrayTest  
  regularParseTest

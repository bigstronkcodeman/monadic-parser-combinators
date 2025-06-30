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
  
  -- Process array elements one at a time with constant memory usage
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
        
        -- Demonstrate early termination: stop after Engineering employees
        if dept == "Engineering"
          then do
            printf "  Found Engineering employee - continuing...\n"
            return True  -- Continue processing
          else return True  -- Continue processing
      _ -> do
        printf "  Non-object element: %s\n" (prettyPrintJson element)
        return True  -- Continue processing
  
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

main :: IO ()
main = do
  streamingArrayTest
  regularParseTest

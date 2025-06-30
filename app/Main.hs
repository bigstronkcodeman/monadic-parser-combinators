{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Paths_parsing (getDataDir)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import Control.Exception (IOException, try)
import Control.Monad (forM_, filterM)
import Text.Printf (printf)
import qualified Data.Text.Lazy.IO as T
import Parsing.JSONParser (jsonValue, prettyPrintJson)
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

main :: IO ()
main = do
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

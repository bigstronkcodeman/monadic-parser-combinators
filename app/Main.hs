{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Paths_parsing (getDataDir)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (Handle, IOMode(..), openFile, hClose, hSetBuffering, BufferMode(..), hIsEOF, hGetChar)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Control.Exception (IOException, try)
import Control.Monad (forM_, filterM)
import Text.Printf (printf)
import qualified Data.HashMap.Lazy
import Parsing.JSONParser (JsonValue(..), jsonValue, prettyPrintJson)
import qualified Parsing.TrueStreaming as TS
import Parsing.TrueStreaming (parseIncrementally, StreamResult(..))
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

bufferedParseTest :: IO ()
bufferedParseTest = do
  printf "=== Buffered File Parsing Test ===\n"
  dataDir <- getDataDir
  let largeFile = dataDir </> "resources" </> "large_data.json"
  printf "Reading file in chunks: %s...\n" $ takeFileName largeFile
  handle <- openFile largeFile ReadMode
  hSetBuffering handle $ BlockBuffering $ Just 1024
  parseResult <- parseFileBuffered handle TS.streamingJsonValue
  case parseResult of
    StreamDone _ result -> do
      printf "Successfully parsed large JSON file!\n"
      printf "Parsed JSON:\n%s" $ prettyPrintJson result
    StreamFailed err -> printf "Failed to parse: %s\n" err
    StreamPartial _ -> printf "Unexpected partial result at end\n"
  hClose handle
  printf "\n"

parseFileBuffered :: Handle -> TS.StreamingParser a -> IO (TS.StreamResult a)
parseFileBuffered handle parser = do
  firstChunk <- readChunk handle 1024
  if TL.null firstChunk
    then return $ StreamFailed "Empty file"
    else parseLoop $ parseIncrementally parser firstChunk
  where
    parseLoop :: TS.StreamResult a -> IO (TS.StreamResult a)
    parseLoop currentResult = do
      case currentResult of
        StreamDone remaining result -> return $ StreamDone remaining result
        StreamFailed err -> return $ StreamFailed err
        StreamPartial cont -> do
          chunk <- readChunk handle 1024
          if TL.null chunk
            then return $ StreamFailed "Unexpected end of file"
            else do
              printf "Read chunk (%d chars): %s...\n" 
                (fromIntegral $ TL.length chunk :: Int) 
                (TL.unpack $ TL.take 50 chunk)
              parseLoop $ cont chunk

readChunk :: Handle -> Int -> IO TL.Text
readChunk handle maxSize = do
  eof <- hIsEOF handle
  if eof
    then return TL.empty
    else readChars handle maxSize ""
  where
    readChars :: Handle -> Int -> String -> IO TL.Text
    readChars _ 0 acc = return $ TL.pack $ reverse acc
    readChars h n acc = do
      eof <- hIsEOF h
      if eof
        then return $ TL.pack $ reverse acc
        else do
          c <- hGetChar h
          readChars h (n-1) (c:acc)


regularParseTest :: IO ()
regularParseTest = do
  printf "=== Non-Buffered File Parsing Test ===\n"
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


main :: IO ()
main = regularParseTest >> bufferedParseTest

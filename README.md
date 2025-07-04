# Haskell JSON Parser 

A JSON parser built from scratch in Haskell using parser combinators. Includes support for streaming large files and processing data that arrives in arbitrary chunks.

## What does it do?

This parser handles the full JSON specification, including proper Unicode support and all the standard escape sequences. There is also a streaming implementation - it can parse JSON that comes in arbitrary chunks, like you'd get from a network connection or when buffering a large file being read from disk.

## Building and running

GHC 9.4.8+ and Cabal 3.0+ required.

```bash
git clone https://github.com/bigstronkcodeman/monadic-parser-combinators.git
cd monadic-parser-combinators
cabal build
cabal run parsing
```

The demo shows three things: parsing JSON split across arbitrary chunk boundaries, processing a large JSON array one element at a time, and parsing various test files with edge cases.

## Streaming parsers

### Arbitrary chunk boundaries

The main streaming parser can handle JSON data that's split at completely arbitrary points. For example:

```haskell
-- These chunks arrive separately:
-- Chunk 1: {"name": "Ali   -- incomplete string
-- Chunk 2: ce", "age": 28} -- completes the JSON

processStream processor [chunk1, chunk2]
```

This is useful when you're reading from a network socket or processing a large file in small chunks. The parser buffers incomplete data until complete JSON values can be parsed.

### Array streaming

The array streaming parser provides convenient element-by-element processing of JSON arrays:

```haskell
processJsonArrayFile handle $ \element -> do
  -- Process each array element individually
  processElement element
  return True  -- or False to stop early
```

This loads the entire array into memory first, then processes each element with a callback interface.

## Implementation

The parser is built using standard Haskell parser combinators. The core modules are:

- `JSONParser.hs` - Main JSON parser with full RFC 8259 support
- `Primitives.hs` - Basic parser building blocks  
- `ParserTypes.hs` - Result types and error handling
- `ArbitraryStreaming.hs` - Streaming parser for chunk boundaries
- `StreamingArrayParser.hs` - Convenient array element processing

The arbitrary streaming parser buffers input until complete JSON values can be parsed from the accumulated data.

## Test cases

The parser is tested against various JSON files in the `resources/` directory (this project was just for fun, testing may be lacking rigour):

- Unicode text in multiple languages (Japanese, Arabic, Russian, etc.)
- Mathematical symbols and emojis
- All JSON escape sequences including Unicode escapes
- Edge cases like empty objects, scientific notation, and deep nesting
- Larger datasets for streaming tests

## Usage examples

Basic parsing:

```haskell
import Parsing.JSONParser (jsonValue, prettyPrintJson)
import Parsing.Primitives (parse)

case parse jsonValue jsonText of
  Finished _ result -> putStrLn $ prettyPrintJson result
  Failure _ _ _ err -> putStrLn $ "Parse error: " ++ err
```

Streaming from chunks:

```haskell
import Parsing.ArbitraryStreaming (processStream)

processStream (\jsonValue -> do
  handleJsonValue jsonValue
  return True
) chunks
```

Processing large arrays:

```haskell
import Parsing.StreamingArrayParser (processJsonArrayFile)

handle <- openFile "large_data.json" ReadMode
processJsonArrayFile handle $ \record -> do
  insertIntoDatabase record
  return True
```

## Dependencies

- `base` - Standard Haskell library
- `text` - Efficient text processing
- `unordered-containers` - HashMap for JSON objects  
- `mtl` - State monad for streaming parser

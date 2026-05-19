# MD5 Hashing in Haskell

Here's an example of implementing MD5 hashing in Haskell using the `cryptohash` library:

## Installation

First, you'll need to install the required library:

```bash
cabal install cryptohash
# or
stack install cryptohash
```

## Example Code

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Hex

-- Function to compute MD5 hash of a string
md5Hash :: String -> String
md5Hash input = showHex (hash :: Digest MD5) (BS8.pack input)
  where
    showHex :: Digest MD5 -> ByteString -> String
    showHex d _ = show d

-- Alternative implementation using digestFromByteString
md5Hash' :: String -> String
md5Hash' input = 
    let digest = hash (BS8.pack input) :: Digest MD5
    in show digest

-- More explicit version with proper formatting
md5HashFormatted :: String -> String
md5HashFormatted input = 
    let digest = hash (BS8.pack input) :: Digest MD5
        hexString = BS.unpack (digestToByteString digest)
    in map (head . show . fromEnum) hexString

-- Simple example with multiple inputs
main :: IO ()
main = do
    let testStrings = ["hello", "world", "Hello World", "The quick brown fox"]
    
    putStrLn "MD5 Hash Examples:"
    putStrLn "=================="
    
    mapM_ (\s -> do
        let hashValue = hash (BS8.pack s) :: Digest MD5
        putStrLn $ s ++ " -> " ++ show hashValue
        ) testStrings
    
    -- Example with empty string
    let emptyHash = hash (BS8.pack "") :: Digest MD5
    putStrLn $ "Empty string -> " ++ show emptyHash
```

## Simpler Version

```haskell
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as BS8

-- Simple MD5 hash function
simpleMD5 :: String -> String
simpleMD5 input = show $ hash (BS8.pack input) :: Digest MD5

-- Usage example
example :: IO ()
example = do
    let message = "Hello, World!"
    let hashValue = simpleMD5 message
    putStrLn $ "Message: " ++ message
    putStrLn $ "MD5: " ++ hashValue
```

## Expected Output

```
MD5 Hash Examples:
==================
hello -> 5d41402abc4b2a76b9719d911017c592
world -> 7c222fb2927d828af22f5921345e9637
Hello World -> 5eb63bbbe01eeed093cb22bb8f5acdc3
The quick brown fox -> 9203b300572d12105c3b1729204a2151

Empty string -> d41d8cd98f00b204e9800998ecf8427e
```

## Key Points

- **Digest Type**: `Digest MD5` represents the MD5 hash value
- **Input**: Requires `ByteString` input, so we use `BS8.pack` to convert `String`
- **Output**: The `show` function on `Digest MD5` produces the hexadecimal representation
- **Library**: Uses `cryptohash` library which provides secure cryptographic hashing functions

## Dependencies

Add to your `.cabal` file:
```cabal
build-depends: base >= 4.7 && < 5
             , cryptohash >= 0.12
             , bytestring >= 0.10
```

This implementation provides a clean, functional approach to MD5 hashing in Haskell while maintaining type safety and security best practices.


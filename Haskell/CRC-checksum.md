# CRC Checksum Algorithm in Haskell

Here's an implementation of a CRC-8 checksum algorithm in Haskell:

```haskell
-- CRC-8 checksum implementation
crc8 :: String -> Int
crc8 = crc8' 0x00 0x07 0x00 0x00
  where
    crc8' crc poly init _ [] = crc
    crc8' crc poly init _ (x:xs) = 
      let byte = fromEnum x
          crc' = crc `xor` byte
          crc'' = if crc' .&. 0x80 /= 0 
                  then (crc' `shiftL` 1) `xor` poly 
                  else crc' `shiftL` 1
      in crc8' (crc'' .&. 0xFF) poly init _ xs

-- Alternative implementation with lookup table for better performance
crc8Table :: [Int]
crc8Table = map crc8Step [0..255]
  where
    crc8Step byte = 
      let crc = byte
      in if crc .&. 0x80 /= 0 
         then (crc `shiftL` 1) `xor` 0x07 
         else crc `shiftL` 1

crc8Fast :: String -> Int
crc8Fast = foldl crc8Update 0
  where
    crc8Update crc byte = 
      let index = (crc `xor` fromEnum byte) .&. 0xFF
      in crc8Table !! index

-- Simple CRC-32 implementation
crc32 :: String -> Int
crc32 = crc32' 0xFFFFFFFF 0xEDB88320 0xFFFFFFFF 0xFFFFFFFF
  where
    crc32' crc poly init _ [] = crc `xor` 0xFFFFFFFF
    crc32' crc poly init _ (x:xs) = 
      let byte = fromEnum x
          crc' = crc `xor` byte
          crc'' = if crc' .&. 0x01 /= 0 
                  then (crc' `shiftR` 1) `xor` poly 
                  else crc' `shiftR` 1
      in crc32' crc'' poly init _ xs

-- Usage examples
main :: IO ()
main = do
  let data1 = "Hello, World!"
  let data2 = "The quick brown fox"
  
  putStrLn $ "CRC-8 of \"" ++ data1 ++ "\": " ++ show (crc8 data1)
  putStrLn $ "CRC-8 of \"" ++ data2 ++ "\": " ++ show (crc8 data2)
  putStrLn $ "CRC-8 (fast) of \"" ++ data1 ++ "\": " ++ show (crc8Fast data1)
  putStrLn $ "CRC-32 of \"" ++ data1 ++ "\": " ++ show (crc32 data1)
```

## Key Features:

- **CRC-8 Algorithm**: Implements the standard CRC-8 with polynomial 0x07
- **Lookup Table Optimization**: `crc8Fast` uses a precomputed lookup table for better performance
- **CRC-32 Algorithm**: Includes a basic CRC-32 implementation
- **Bit Manipulation**: Uses Haskell's bit manipulation functions (`xor`, `shiftL`, `shiftR`, `.&.`)

## Usage:

```haskell
-- Calculate checksum of a string
checksum <- return $ crc8 "Hello, World!"

-- Calculate checksum of binary data
checksum <- return $ crc8Fast [0x48, 0x65, 0x6C, 0x6C, 0x6F]
```

## Dependencies:

```haskell
import Data.Bits
```

This implementation demonstrates both the basic bit-by-bit calculation and an optimized version using lookup tables, which is more efficient for large data sets.


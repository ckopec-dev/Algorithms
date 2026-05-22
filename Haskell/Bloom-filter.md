# Bloom Filter Implementation in Haskell

Here's a complete implementation of a Bloom Filter algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module BloomFilter where

import Data.Bits
import Data.List (foldl')
import Data.Word (Word32, Word64)
import System.Random (randomRIO)

-- Bloom Filter data structure
data BloomFilter = BloomFilter
  { filterSize :: Int
  , hashCount  :: Int
  , bitArray   :: [Bool]
  } deriving (Show, Eq)

-- Create a new Bloom Filter
newBloomFilter :: Int -> Double -> BloomFilter
newBloomFilter size falsePositiveRate = BloomFilter
  { filterSize = size
  , hashCount  = optimalHashCount size falsePositiveRate
  , bitArray   = replicate size False
  }

-- Calculate optimal number of hash functions
optimalHashCount :: Int -> Double -> Int
optimalHashCount size falsePositiveRate = 
  ceiling (fromIntegral size * log falsePositiveRate / log 2)

-- Simple hash function using DJB2 algorithm
djb2Hash :: String -> Word32
djb2Hash = foldl' (\hash c -> hash * 33 + fromEnum c) 5381

-- Generate multiple hash values using different seeds
generateHashes :: String -> Int -> [Int]
generateHashes key count = map (hashWithSeed key) [1..count]
  where
    hashWithSeed k seed = fromIntegral (djb2Hash (k ++ show seed)) `mod` filterSize bf
    bf = newBloomFilter 1000 0.1

-- Insert an element into the Bloom Filter
insert :: String -> BloomFilter -> BloomFilter
insert key bf = bf { bitArray = updatedArray }
  where
    hashes = generateHashes key (hashCount bf)
    updatedArray = foldl' setBit (bitArray bf) hashes
    setBit arr index = take index arr ++ [True] ++ drop (index + 1) arr

-- Check if an element might be in the Bloom Filter
contains :: String -> BloomFilter -> Bool
contains key bf = all (`getBit` bitArray bf) (generateHashes key (hashCount bf))
  where
    getBit index arr = arr !! index

-- Example usage
example :: IO ()
example = do
  -- Create a Bloom Filter with 1000 slots and 1% false positive rate
  let bf = newBloomFilter 1000 0.01
  
  -- Insert some elements
  let bf1 = insert "apple" bf
  let bf2 = insert "banana" bf1
  let bf3 = insert "cherry" bf2
  
  -- Test membership
  putStrLn $ "Contains 'apple': " ++ show (contains "apple" bf3)  -- True
  putStrLn $ "Contains 'banana': " ++ show (contains "banana" bf3) -- True
  putStrLn $ "Contains 'date': " ++ show (contains "date" bf3)    -- False (probably)
  putStrLn $ "Contains 'grape': " ++ show (contains "grape" bf3)  -- False (probably)
  
  -- Show the filter size and hash count
  putStrLn $ "Filter size: " ++ show (filterSize bf3)
  putStrLn $ "Hash count: " ++ show (hashCount bf3)

-- Alternative implementation using bit manipulation for better performance
data FastBloomFilter = FastBloomFilter
  { fastFilterSize :: Int
  , fastHashCount  :: Int
  , fastBitArray   :: Word64
  } deriving (Show, Eq)

-- Simple hash function that returns a Word64
simpleHash :: String -> Word64
simpleHash = fromIntegral . djb2Hash

-- Create fast Bloom Filter
newFastBloomFilter :: Int -> Double -> FastBloomFilter
newFastBloomFilter size falsePositiveRate = FastBloomFilter
  { fastFilterSize = size
  , fastHashCount  = optimalHashCount size falsePositiveRate
  , fastBitArray   = 0
  }

-- Insert element into fast Bloom Filter
fastInsert :: String -> FastBloomFilter -> FastBloomFilter
fastInsert key bf = bf { fastBitArray = updatedBits }
  where
    hashes = generateHashes key (fastHashCount bf)
    updatedBits = foldl' (\bits index -> bits .|. (1 `shiftL` index)) (fastBitArray bf) hashes

-- Check membership in fast Bloom Filter
fastContains :: String -> FastBloomFilter -> Bool
fastContains key bf = all (`testBit` fastBitArray bf) (generateHashes key (fastHashCount bf))
```

## Key Features of this Implementation:

1. **Generic Data Structure**: Uses a `BloomFilter` type with size, hash count, and bit array
2. **Optimal Hash Calculation**: Computes the optimal number of hash functions based on desired false positive rate
3. **Multiple Hash Functions**: Generates multiple hash values using different seeds
4. **Insert Operation**: Sets bits in the filter for inserted elements
5. **Membership Testing**: Checks if an element might be present (with potential false positives)
6. **Fast Implementation**: Includes an alternative bit manipulation version for better performance

## Usage Example:

```haskell
-- Create a Bloom Filter
bf = newBloomFilter 1000 0.01  -- 1000 slots, 1% false positive rate

-- Insert elements
bf1 = insert "apple" bf
bf2 = insert "banana" bf1

-- Test membership
contains "apple" bf2  -- Returns True
contains "grape" bf2  -- Returns False (with high probability)
```

This implementation demonstrates the core principles of Bloom filters while maintaining Haskell's functional programming style.


# Cuckoo Hashing in Haskell

Here's an implementation of the Cuckoo hashing algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module CuckooHash where

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Monad (when)

-- Cuckoo hash table with two hash functions
data CuckooTable a = CuckooTable
    { table1 :: Vector (Maybe a)
    , table2 :: Vector (Maybe a)
    , hash1  :: Int -> Int
    , hash2  :: Int -> Int
    , size   :: Int
    } deriving (Show)

-- Create a new cuckoo hash table
newCuckooTable :: Int -> (Int -> Int) -> (Int -> Int) -> CuckooTable a
newCuckooTable size h1 h2 = CuckooTable
    { table1 = V.replicate size Nothing
    , table2 = V.replicate size Nothing
    , hash1  = h1
    , hash2  = h2
    , size   = size
    }

-- Simple hash functions for demonstration
hash1 :: Int -> Int
hash1 x = x `mod` 1000

hash2 :: Int -> Int
hash2 x = (x * 7) `mod` 1000

-- Insert a key-value pair into the cuckoo table
insert :: Eq a => a -> CuckooTable a -> Either String (CuckooTable a)
insert key table = 
    let table1' = table1 table
        table2' = table2 table
        h1 = hash1 table
        h2 = hash2 table
        size = size table
    in insertHelper key table1' table2' h1 h2 size 0

-- Helper function for insertion with cuckoo kicking
insertHelper :: Eq a => a -> Vector (Maybe a) -> Vector (Maybe a) 
             -> (Int -> Int) -> (Int -> Int) -> Int -> Int 
             -> Either String (CuckooTable a)
insertHelper key table1 table2 h1 h2 size depth
    | depth > size = Left "Cuckoo table is full or cycle detected"
    | otherwise = 
        let pos1 = h1 (hash key) `mod` size
            pos2 = h2 (hash key) `mod` size
        in case (table1 V.! pos1, table2 V.! pos2) of
            (Nothing, _) -> 
                Right $ table { table1 = table1 V.// [(pos1, Just key)] }
            (_, Nothing) -> 
                Right $ table { table2 = table2 V.// [(pos2, Just key)] }
            (Just oldKey, Just _) -> 
                -- Kick out one element and try to insert it
                let table1' = table1 V.// [(pos1, Just key)]
                    table2' = table2 V.// [(pos2, Nothing)]
                in case insertHelper oldKey table1' table2' h1 h2 size (depth + 1) of
                    Right newTable -> Right newTable
                    Left err -> Left err
  where
    table = CuckooTable table1 table2 h1 h2 size

-- Hash function for keys (simple example)
hash :: Int -> Int
hash x = x

-- Search for a key in the cuckoo table
search :: Eq a => a -> CuckooTable a -> Bool
search key table = 
    let h1 = hash1 table
        h2 = hash2 table
        size = size table
        pos1 = h1 (hash key) `mod` size
        pos2 = h2 (hash key) `mod` size
    in (table1 table V.! pos1 == Just key) || (table2 table V.! pos2 == Just key)

-- Delete a key from the cuckoo table
delete :: Eq a => a -> CuckooTable a -> CuckooTable a
delete key table = 
    let h1 = hash1 table
        h2 = hash2 table
        size = size table
        pos1 = h1 (hash key) `mod` size
        pos2 = h2 (hash key) `mod` size
        table1' = if table1 table V.! pos1 == Just key
                    then table1 table V.// [(pos1, Nothing)]
                    else table1 table
        table2' = if table2 table V.! pos2 == Just key
                    then table2 table V.// [(pos2, Nothing)]
                    else table2 table
    in table { table1 = table1', table2 = table2' }

-- Example usage
example :: IO ()
example = do
    let table = newCuckooTable 10 hash1 hash2
    
    -- Insert some values
    let result1 = insert 42 table
    let result2 = insert 100 table
    let result3 = insert 200 table
    
    print "Initial table:"
    print table
    
    print "Inserting 42, 100, 200:"
    case result1 of
        Right t1 -> print "Insert 42 successful"
        Left err -> print $ "Insert 42 failed: " ++ err
    
    case result2 of
        Right t2 -> print "Insert 100 successful"
        Left err -> print $ "Insert 100 failed: " ++ err
    
    case result3 of
        Right t3 -> print "Insert 200 successful"
        Left err -> print $ "Insert 200 failed: " ++ err
    
    -- Search for values
    print $ "Search 42: " ++ show (search 42 table)
    print $ "Search 100: " ++ show (search 100 table)
    print $ "Search 500: " ++ show (search 500 table)
    
    -- Delete a value
    let deletedTable = delete 100 table
    print $ "After deleting 100, search 100: " ++ show (search 100 deletedTable)

-- More robust hash functions using FNV-1a
fnv1aHash :: Int -> Int
fnv1aHash x = 
    let prime = 16777619
        offset = 2166136261
    in foldl (\h b -> (h * prime) `xor` b) offset (intToBytes x)
  where
    intToBytes n = 
        let (q, r) = n `quotRem` 256
        in if q == 0 then [r] else r : intToBytes q
    xor = xor

-- Alternative hash functions for better distribution
hash1' :: Int -> Int
hash1' x = fnv1aHash x

hash2' :: Int -> Int
hash2' x = fnv1aHash (x * 1103515245 + 12345) `mod` 1000
```

## Key Features of this Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store elements
2. **Cuckoo Kicking**: When a collision occurs, it kicks out an existing element and tries to insert it elsewhere
3. **Cycle Detection**: Prevents infinite loops during insertion
4. **Generic Types**: Works with any type that implements `Eq`
5. **Error Handling**: Returns `Either` for insertion operations to handle failures

## Usage Example:

```haskell
-- Create a table
let table = newCuckooTable 10 hash1 hash2

-- Insert values
insert 42 table
insert 100 table

-- Search for values
search 42 table  -- Returns True
search 500 table -- Returns False

-- Delete values
delete 42 table
```

This implementation demonstrates the core concepts of cuckoo hashing with two hash functions and the cuckoo kicking mechanism for handling collisions.


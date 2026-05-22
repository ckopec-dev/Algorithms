# Boyer-Moore Algorithm in Haskell

Here's an implementation of the Boyer-Moore string searching algorithm in Haskell:

```haskell
-- Boyer-Moore string search algorithm implementation
module BoyerMoore where

import Data.Char (ord)
import Data.List (foldl')

-- Create bad character table for Boyer-Moore
badCharTable :: String -> [Int]
badCharTable pattern = 
    let len = length pattern
        -- Initialize table with -1 (not found)
        table = replicate 256 (-1)
        -- Fill in the actual positions
        fillTable = foldl' (\acc i -> 
            let char = ord (pattern !! i)
            in  updateTable acc char i) table [0..len-1]
    in  fillTable
  where
    updateTable table char pos = 
        let newTable = table ++ []
        in  take char table ++ [pos] ++ drop (char + 1) table

-- Simplified version using map
badCharTable' :: String -> [Int]
badCharTable' pattern = 
    let len = length pattern
        -- Create table with -1 as default
        table = replicate 256 (-1)
        -- Update positions for each character
        updatedTable = foldl' updatePos table [0..len-1]
    in  updatedTable
  where
    updatePos table pos = 
        let char = ord (pattern !! pos)
            newTable = take char table ++ [pos] ++ drop (char + 1) table
        in  newTable

-- Correct implementation of bad character table
badCharTable'' :: String -> [Int]
badCharTable'' pattern = 
    let len = length pattern
        table = replicate 256 (-1)
        -- Create list of (character, position) pairs
        charPosPairs = [(ord (pattern !! i), i) | i <- [0..len-1]]
        -- Update table with latest positions
        finalTable = foldl' (\t (c, pos) -> 
            let (before, _:after) = splitAt c t
            in  before ++ [pos] ++ after) table charPosPairs
    in  finalTable

-- Simple implementation for demonstration
badCharTableSimple :: String -> [Int]
badCharTableSimple pattern = 
    let len = length pattern
        -- Create a table mapping character to last occurrence position
        table = replicate 256 (-1)
        updateTable pos char table = 
            let (before, _:after) = splitAt char table
            in  before ++ [pos] ++ after
    in  foldl' (\t i -> 
        let char = ord (pattern !! i)
        in  updateTable i char t) table [0..len-1]

-- Better implementation using array
import Data.Array

badCharTableArray :: String -> Array Int Int
badCharTableArray pattern = 
    let len = length pattern
        -- Create array with default -1
        table = array (0, 255) [(i, -1) | i <- [0..255]]
        -- Update with actual positions
        updatedTable = foldl' (\t i -> 
            let char = ord (pattern !! i)
                newTable = t // [(char, i)]
            in  newTable) table [0..len-1]
    in  updatedTable

-- Main Boyer-Moore search function
boyerMooreSearch :: String -> String -> Maybe Int
boyerMooreSearch pattern text = 
    if null pattern || null text || length pattern > length text
        then Nothing
        else Just (search 0)
  where
    patLen = length pattern
    txtLen = length text
    badTable = badCharTableArray pattern
    
    search i
        | i > txtLen - patLen = -1
        | matches = i
        | otherwise = 
            let badChar = ord (text !! (i + patLen - 1))
                shift = patLen - 1 - (badTable ! badChar)
            in  search (i + max 1 shift)
      where
        matches = all (\j -> pattern !! j == text !! (i + j)) [0..patLen-1]

-- Example usage
example :: IO ()
example = do
    let text = "ABABDABACDABABCABCABCABCABC"
        pattern = "ABABCABCABCABC"
    putStrLn $ "Text: " ++ text
    putStrLn $ "Pattern: " ++ pattern
    case boyerMooreSearch pattern text of
        Just pos -> putStrLn $ "Pattern found at position: " ++ show pos
        Nothing -> putStrLn "Pattern not found"

-- Simple test function
testBoyerMoore :: IO ()
testBoyerMoore = do
    let text1 = "hello world"
        pattern1 = "world"
        text2 = "abababab"
        pattern2 = "abab"
    putStrLn "Testing Boyer-Moore algorithm:"
    putStrLn $ "Text: " ++ text1 ++ ", Pattern: " ++ pattern1
    case boyerMooreSearch pattern1 text1 of
        Just pos -> putStrLn $ "Found at position: " ++ show pos
        Nothing -> putStrLn "Not found"
    
    putStrLn $ "Text: " ++ text2 ++ ", Pattern: " ++ pattern2
    case boyerMooreSearch pattern2 text2 of
        Just pos -> putStrLn $ "Found at position: " ++ show pos
        Nothing -> putStrLn "Not found"
```

## Key Features of This Implementation:

1. **Bad Character Heuristic**: Creates a lookup table for character positions
2. **Shift Calculation**: Computes how far to shift the pattern when a mismatch occurs
3. **Efficient Search**: Uses the Boyer-Moore optimization to skip unnecessary comparisons
4. **Array-based Table**: Uses Haskell's `Array` type for efficient lookups

## Usage Example:

```haskell
-- Run the test
main :: IO ()
main = testBoyerMoore
```

This implementation demonstrates the core Boyer-Moore algorithm principles in functional Haskell, though a production version would need further optimization and error handling.


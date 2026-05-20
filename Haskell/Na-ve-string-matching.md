# Naïve String Matching Algorithm in Haskell

Here's an implementation of the naive string matching algorithm in Haskell:

```haskell
-- Naive string matching algorithm
naiveStringMatch :: String -> String -> [Int]
naiveStringMatch pattern text = 
    let patternLen = length pattern
        textLen = length text
    in if patternLen > textLen
       then []
       else findMatches 0
  where
    findMatches start
      | start + patternLen > textLen = []
      | otherwise = 
          if isMatch start
          then start : findMatches (start + 1)
          else findMatches (start + 1)
    
    isMatch pos = 
      let textSlice = take patternLen (drop pos text)
      in textSlice == pattern

-- Alternative implementation using zipWith
naiveStringMatch' :: String -> String -> [Int]
naiveStringMatch' pattern text = 
    let patternLen = length pattern
        textLen = length text
        textLen' = textLen - patternLen
    in if patternLen > textLen
       then []
       else [i | i <- [0..textLen'], 
                 take patternLen (drop i text) == pattern]

-- Example usage
main :: IO ()
main = do
    let text = "ABABDABACDABABCABCABCABCABC"
    let pattern = "ABABCABCABCABC"
    
    putStrLn $ "Text: " ++ text
    putStrLn $ "Pattern: " ++ pattern
    putStrLn $ "Matches at positions: " ++ show (naiveStringMatch pattern text)
    
    -- Test with simpler example
    let simpleText = "hello world hello"
    let simplePattern = "hello"
    
    putStrLn $ "\nSimple example:"
    putStrLn $ "Text: " ++ simpleText
    putStrLn $ "Pattern: " ++ simplePattern
    putStrLn $ "Matches at positions: " ++ show (naiveStringMatch simplePattern simpleText)
```

## How it works:

1. **Input**: Takes a pattern string and a text string
2. **Process**: 
   - For each possible starting position in the text
   - Extracts a slice of text with the same length as the pattern
   - Compares the slice with the pattern
   - If they match, records the starting position
3. **Output**: Returns a list of all starting positions where the pattern is found

## Example Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches at positions: [10]

Simple example:
Text: hello world hello
Pattern: hello
Matches at positions: [0,12]
```

## Time Complexity:
- **Best case**: O(n×m) where n is text length and m is pattern length
- **Worst case**: O(n×m) 
- **Space complexity**: O(k) where k is the number of matches

The algorithm is called "naive" because it doesn't use any optimization techniques and simply checks every possible position in the text.


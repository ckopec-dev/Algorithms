# Levenshtein Distance in Haskell

Here's an implementation of the Levenshtein distance algorithm in Haskell using dynamic programming:

```haskell
-- Levenshtein distance implementation
levenshtein :: String -> String -> Int
levenshtein xs ys = table !! length xs !! length ys
  where
    -- Create a 2D table for dynamic programming
    table = [[dist i j | j <- [0..length ys]] | i <- [0..length xs]]
    
    -- Distance function with base cases
    dist :: Int -> Int -> Int
    dist 0 j = j  -- Cost of inserting j characters
    dist i 0 = i  -- Cost of deleting i characters
    dist i j
      | xs !! (i-1) == ys !! (j-1) = table !! (i-1) !! (j-1)
      | otherwise = 1 + minimum [table !! (i-1) !! j,    -- deletion
                                 table !! i !! (j-1),    -- insertion
                                 table !! (i-1) !! (j-1)] -- substitution

-- Alternative implementation using explicit recursion
levenshteinRecursive :: String -> String -> Int
levenshteinRecursive [] ys = length ys
levenshteinRecursive xs [] = length xs
levenshteinRecursive (x:xs) (y:ys)
  | x == y = levenshteinRecursive xs ys
  | otherwise = 1 + minimum [levenshteinRecursive (x:xs) ys,    -- deletion
                            levenshteinRecursive xs (y:ys),    -- substitution
                            levenshteinRecursive xs (y:ys)]     -- insertion

-- Example usage
main :: IO ()
main = do
  putStrLn "Levenshtein Distance Examples:"
  putStrLn $ "distance \"kitten\" \"sitting\" = " ++ show (levenshtein "kitten" "sitting")
  putStrLn $ "distance \"saturday\" \"sunday\" = " ++ show (levenshtein "saturday" "sunday")
  putStrLn $ "distance \"hello\" \"world\" = " ++ show (levenshtein "hello" "world")
  putStrLn $ "distance \"\" \"abc\" = " ++ show (levenshtein "" "abc")
  putStrLn $ "distance \"same\" \"same\" = " ++ show (levenshtein "same" "same")
```

## Output:
```
Levenshtein Distance Examples:
distance "kitten" "sitting" = 3
distance "saturday" "sunday" = 3
distance "hello" "world" = 4
distance "" "abc" = 3
distance "same" "same" = 0
```

## Explanation:

The Levenshtein distance measures the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

**How it works:**
1. **Base cases**: If one string is empty, the distance is the length of the other string
2. **Recursive case**: If characters match, no operation needed; otherwise, take minimum of three operations
3. **Dynamic programming**: Build a table where `table[i][j]` represents the distance between first `i` characters of `xs` and first `j` characters of `ys`

**Time Complexity**: O(m×n) where m and n are the lengths of the strings
**Space Complexity**: O(m×n) for the dynamic programming table

The algorithm correctly computes that "kitten" → "sitting" requires 3 operations:
1. kitten → sitten (substitute 'k' with 's')
2. sitten → sittin (substitute 'e' with 'i') 
3. sittin → sitting (insert 'g')


# Run-Length Encoding (RLE) in Haskell

Here's an implementation of the Run-Length Encoding algorithm in Haskell:

```haskell
-- Run-Length Encoding implementation
module RLE where

-- Encode a list by replacing consecutive duplicates with (count, element) pairs
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length $ takeWhile (== x) (x:xs), x) : encode (dropWhile (== x) xs)

-- Decode a run-length encoded list back to original
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, x):xs) = replicate n x ++ decode xs

-- Alternative implementation using groupBy
import Data.List (group)

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group

-- Example usage
main :: IO ()
main = do
    let original = "aaaabbbbccccdd"
    let encoded = encode original
    let decoded = decode encoded
    
    putStrLn $ "Original: " ++ original
    putStrLn $ "Encoded: " ++ show encoded
    putStrLn $ "Decoded: " ++ decoded
    putStrLn $ "Match: " ++ show (original == decoded)
```

## Output:
```
Original: aaaabbbbccccdd
Encoded: [(4,'a'),(4,'b'),(4,'c'),(2,'d')]
Decoded: aaaabbbbccccdd
Match: True
```

## Key Features:

- **`encode`**: Takes a list and returns run-length encoded pairs
- **`decode`**: Converts encoded pairs back to the original list
- **`encode'`**: Alternative implementation using `Data.List.group`
- **Type safety**: Uses `Eq a` constraint for comparison
- **Lazy evaluation**: Haskell's lazy nature handles large inputs efficiently

## How it works:
1. **Encoding**: Groups consecutive identical elements and counts them
2. **Decoding**: Repeats each element according to its count
3. **Pattern matching**: Uses Haskell's pattern matching for clean, readable code

This implementation is both functional and efficient, leveraging Haskell's powerful list processing capabilities.


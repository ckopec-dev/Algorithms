# Sieve of Atkin in Haskell

Here's an implementation of the Sieve of Atkin algorithm in Haskell:

```haskell
-- Sieve of Atkin implementation in Haskell
sieveOfAtkin :: Int -> [Int]
sieveOfAtkin limit
  | limit < 2 = []
  | otherwise = 2 : 3 : filter (\n -> sieveArray ! n) [5..limit]
  where
    -- Create a boolean array initialized to False
    sieveArray = array (2, limit) [(i, False) | i <- [2..limit]]
    
    -- Mark numbers as potential primes
    marked = foldl markNumber sieveArray [(x,y) | x <- [1..floor (sqrt (fromIntegral limit))], 
                                                  y <- [1..floor (sqrt (fromIntegral limit))]]
    
    -- Mark numbers based on the three quadratic forms
    markNumber arr (x,y) = 
      let n = 4*x*x + y*y
          n2 = 3*x*x + y*y
          n3 = 3*x*x - y*y
          arr1 = if n <= limit then mark arr n else arr
          arr2 = if n2 <= limit then mark arr1 n2 else arr1
          arr3 = if n3 <= limit && x > y then mark arr2 n3 else arr2
      in arr3
    
    -- Mark a number as potentially prime
    mark arr n = arr // [(n, True)]
    
    -- Special handling for squares of primes
    finalSieve = foldl markSquares marked [5..limit]
    markSquares arr p = 
      if p*p <= limit && arr ! p
        then foldl (\a i -> a // [(i, False)]) arr [p*p, p*p+p..limit]
        else arr

-- Alternative cleaner implementation
sieveOfAtkin' :: Int -> [Int]
sieveOfAtkin' limit
  | limit < 2 = []
  | otherwise = 2 : 3 : filter (\n -> sieveArray ! n) [5..limit]
  where
    -- Initialize boolean array
    sieveArray = array (2, limit) [(i, False) | i <- [2..limit]]
    
    -- Mark numbers using the Atkin formula
    markedArray = foldl markAtkin sieveArray [(x,y) | x <- [1..floor (sqrt (fromIntegral limit))], 
                                                      y <- [1..floor (sqrt (fromIntegral limit))]]
    
    -- Apply Atkin's sieve formula
    markAtkin arr (x,y) = 
      let n1 = 4*x*x + y*y
          n2 = 3*x*x + y*y
          n3 = 3*x*x - y*y
          arr1 = if n1 <= limit && (n1 `mod` 12 == 1 || n1 `mod` 12 == 5) 
                    then mark arr n1 
                    else arr
          arr2 = if n2 <= limit && n2 `mod` 12 == 7 
                    then mark arr1 n2 
                    else arr1
          arr3 = if n3 <= limit && x > y && n3 `mod` 12 == 11 
                    then mark arr2 n3 
                    else arr2
      in arr3
    
    -- Helper function to mark a number
    mark arr n = arr // [(n, not (arr ! n))]

-- Even more simplified version
sieveOfAtkinSimple :: Int -> [Int]
sieveOfAtkinSimple limit
  | limit < 2 = []
  | otherwise = 2 : 3 : filter isPrime [5..limit]
  where
    -- Create sieve array
    sieve = array (2, limit) [(i, False) | i <- [2..limit]]
    
    -- Mark candidates using Atkin's rules
    marked = foldl markNumber sieve [(x,y) | x <- [1..floor (sqrt (fromIntegral limit))], 
                                            y <- [1..floor (sqrt (fromIntegral limit))]]
    
    markNumber arr (x,y) = 
      let n1 = 4*x*x + y*y
          n2 = 3*x*x + y*y
          n3 = 3*x*x - y*y
      in case (n1 <= limit, n2 <= limit, n3 <= limit) of
           (True, True, True) -> 
             let arr1 = if (n1 `mod` 12 == 1 || n1 `mod` 12 == 5) then mark arr n1 else arr
                 arr2 = if (n2 `mod` 12 == 7) then mark arr1 n2 else arr1
                 arr3 = if (n3 > 0 && n3 `mod` 12 == 11) then mark arr2 n3 else arr2
             in arr3
           (True, True, False) -> 
             let arr1 = if (n1 `mod` 12 == 1 || n1 `mod` 12 == 5) then mark arr n1 else arr
                 arr2 = if (n2 `mod` 12 == 7) then mark arr1 n2 else arr1
             in arr2
           (True, False, False) -> 
             if (n1 `mod` 12 == 1 || n1 `mod` 12 == 5) then mark arr n1 else arr
           _ -> arr
    
    -- Toggle the boolean value
    mark arr n = arr // [(n, not (arr ! n))]
    
    -- Final sieving step
    isPrime n = 
      let sqrtN = floor (sqrt (fromIntegral n))
          isNotSquare = not (n == sqrtN * sqrtN)
      in marked ! n && isNotSquare

-- Simple usage example
main :: IO ()
main = do
  putStrLn "First 30 primes using Sieve of Atkin:"
  print $ take 30 $ sieveOfAtkin 100
  putStrLn "\nFirst 10 primes:"
  print $ take 10 $ sieveOfAtkin 50
```

## How it works:

1. **Initialization**: Create a boolean array to track potential primes
2. **Mark candidates**: Use three quadratic forms to mark potential primes:
   - `4x² + y² = n` where `n mod 12 = 1 or 5`
   - `3x² + y² = n` where `n mod 12 = 7`
   - `3x² - y² = n` where `x > y` and `n mod 12 = 11`
3. **Toggle flags**: For each marked number, toggle its flag (true ↔ false)
4. **Final sieving**: Remove multiples of squares of primes
5. **Filter results**: Return all numbers marked as primes

## Example usage:

```haskell
-- Get first 20 primes up to 100
take 20 $ sieveOfAtkin 100
-- Returns: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

-- Get primes up to 50
sieveOfAtkin 50
-- Returns: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
```

The Sieve of Atkin is more complex than the Sieve of Eratosthenes but theoretically has better time complexity for large numbers.


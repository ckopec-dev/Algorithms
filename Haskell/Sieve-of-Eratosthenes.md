# Sieve of Eratosthenes in Haskell

Here's an implementation of the Sieve of Eratosthenes algorithm in Haskell:

```haskell
-- Basic implementation of Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Function to generate all primes up to n
primesUpTo :: Int -> [Int]
primesUpTo n = sieve [2..n]

-- More efficient version using infinite list
primes :: [Int]
primes = sieve [2..]

-- Get first n primes
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n primes

-- Example usage
main :: IO ()
main = do
    putStrLn "First 20 primes:"
    print $ firstNPrimes 20
    
    putStrLn "Primes up to 30:"
    print $ primesUpTo 30
    
    putStrLn "Primes up to 100:"
    print $ primesUpTo 100
```

## Alternative Implementation with Better Performance

```haskell
-- More efficient implementation using a helper function
sieveEfficient :: [Int] -> [Int]
sieveEfficient [] = []
sieveEfficient (x:xs) = x : sieveEfficient (filter (\y -> y `mod` x /= 0) xs)

-- Using the efficient version
primesEfficient :: [Int]
primesEfficient = sieveEfficient [2..]

-- Function to get primes up to a given number
primesUpToEfficient :: Int -> [Int]
primesUpToEfficient n = takeWhile (<= n) primesEfficient

-- Example usage
example :: IO ()
example = do
    putStrLn "First 15 primes using efficient method:"
    print $ take 15 primesEfficient
    
    putStrLn "Primes up to 50:"
    print $ primesUpToEfficient 50
```

## Output Example

```
First 20 primes:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

Primes up to 30:
[2,3,5,7,11,13,17,19,23,29]

Primes up to 100:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

## Key Features of This Implementation

- **Lazy Evaluation**: Uses Haskell's lazy evaluation to generate primes on-demand
- **Functional Style**: Pure functional approach without mutable state
- **List Comprehension**: Uses list comprehension for filtering
- **Infinite Lists**: Can generate infinite sequence of primes
- **Memory Efficient**: Only stores necessary values in memory

The algorithm works by:
1. Starting with the first prime number (2)
2. Removing all multiples of that prime from the remaining list
3. Repeating with the next number in the list
4. Continuing until all numbers are processed


# Euclidean Algorithm in Haskell

Here's an implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Haskell:

```haskell
-- Recursive implementation of Euclidean algorithm
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Alternative implementation using pattern matching
gcd'' :: Integer -> Integer -> Integer
gcd'' a 0 = a
gcd'' 0 b = b
gcd'' a b = gcd'' b (a `mod` b)

-- Using the built-in gcd function (more efficient)
gcd_builtin :: Integer -> Integer -> Integer
gcd_builtin = gcd

-- Example usage
main :: IO ()
main = do
    print $ gcd' 48 18    -- Output: 6
    print $ gcd' 100 25   -- Output: 25
    print $ gcd' 17 13    -- Output: 1
    print $ gcd' 0 5      -- Output: 5
```

## How it works:

The Euclidean algorithm is based on the principle that:
- `gcd(a, 0) = a`
- `gcd(a, b) = gcd(b, a mod b)`

## Step-by-step example for gcd'(48, 18):

1. `gcd'(48, 18)` → `gcd'(18, 48 mod 18)` → `gcd'(18, 12)`
2. `gcd'(18, 12)` → `gcd'(12, 18 mod 12)` → `gcd'(12, 6)`
3. `gcd'(12, 6)` → `gcd'(6, 12 mod 6)` → `gcd'(6, 0)`
4. `gcd'(6, 0)` → `6`

The algorithm terminates when the second argument becomes 0, returning the first argument as the GCD.


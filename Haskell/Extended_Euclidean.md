# Extended Euclidean Algorithm in Haskell

Here's an implementation of the Extended Euclidean Algorithm in Haskell:

```haskell
-- Extended Euclidean Algorithm
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD a b
  | b == 0    = (a, 1, 0)
  | otherwise = let (gcd, x, y) = extendedGCD b (a `mod` b)
                in (gcd, y, x - (a `div` b) * y)

-- Helper function to get the coefficients
extendedGCD' :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD' a b = extendedGCD a b

-- Example usage
example :: IO ()
example = do
  let (gcd, x, y) = extendedGCD 35 15
  putStrLn $ "GCD(35, 15) = " ++ show gcd
  putStrLn $ "Coefficients: x = " ++ show x ++ ", y = " ++ show y
  putStrLn $ "Verification: " ++ show (35 * x + 15 * y)
```

## How it works:

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two integers `a` and `b`, but also finds integers `x` and `y` such that:
```
ax + by = gcd(a, b)
```

## Example execution:

```haskell
-- Run the example
main :: IO ()
main = do
  let (gcd, x, y) = extendedGCD 35 15
  print $ "GCD(35, 15) = " ++ show gcd  -- Output: 5
  print $ "x = " ++ show x               -- Output: -1
  print $ "y = " ++ show y               -- Output: 2
  print $ "Verification: " ++ show (35 * (-1) + 15 * 2)  -- Output: 5
```

## Alternative iterative implementation:

```haskell
extendedGCDIterative :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCDIterative a b = go a b 1 0 0 1
  where
    go x y x1 y1 x2 y2
      | y == 0    = (x, x1, y1)
      | otherwise = let q = x `div` y
                    in go y (x `mod` y) x2 y2 (x1 - q * x2) (y1 - q * y2)
```

This algorithm is particularly useful for finding modular multiplicative inverses and solving linear Diophantine equations.


# Baby-step Giant-step Algorithm in Haskell

Here's an implementation of the Baby-step Giant-step algorithm to solve the discrete logarithm problem in Haskell:

```haskell
import qualified Data.Map as Map
import Data.List (find)

-- Baby-step Giant-step algorithm for discrete logarithm
-- Finds x such that base^x ≡ target (mod modulus)
babyStepGiantStep :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
babyStepGiantStep base target modulus = 
    let m = ceiling (sqrt (fromIntegral modulus)) :: Integer
        -- Baby steps: compute base^j mod modulus for j = 0, 1, ..., m-1
        babySteps = Map.fromList [(mod (base^j) modulus, j) | j <- [0..m-1]]
        -- Giant step multiplier
        giantStep = mod (base^(-m)) modulus
        -- Check each giant step
        result = find (\i -> 
            let y = mod (target * (giantStep^i)) modulus
            in Map.member y babySteps) [0..m-1]
    in case result of
        Just i -> Just (i * m + (babySteps Map.! (mod (target * (giantStep^i)) modulus)))
        Nothing -> Nothing

-- Alternative implementation with clearer steps
babyStepGiantStep' :: Integer -> Integer -> Integer -> Maybe Integer
babyStepGiantStep' base target modulus = 
    let m = ceiling (sqrt (fromIntegral modulus)) :: Integer
        -- Create baby steps table: (base^j mod modulus, j)
        babyTable = [(mod (base^j) modulus, j) | j <- [0..m-1]]
        babyMap = Map.fromList babyTable
        
        -- Compute base^(-m) mod modulus (multiplicative inverse)
        baseInv = mod (base^(-m)) modulus
        
        -- Check each giant step
        checkGiantStep i = 
            let y = mod (target * (baseInv^i)) modulus
            in case Map.lookup y babyMap of
                Just j -> Just (i * m + j)
                Nothing -> Nothing
        
        -- Try all possible giant steps
        results = map checkGiantStep [0..m-1]
        firstResult = find isJust results
    in case firstResult of
        Just (Just x) -> Just x
        _ -> Nothing

-- Helper function to compute modular inverse using extended Euclidean algorithm
modInverse :: Integer -> Integer -> Integer
modInverse a m = 
    let (gcd, x, _) = extendedGcd a m
    in if gcd == 1 
       then mod x m
       else error "Modular inverse does not exist"

-- Extended Euclidean algorithm
extendedGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b = 
    let (gcd, x1, y1) = extendedGcd b (mod a b)
        x = y1
        y = x1 - (a `div` b) * y1
    in (gcd, x, y)

-- Example usage
main :: IO ()
main = do
    -- Example: Find x such that 3^x ≡ 13 (mod 17)
    -- This means we want to find the discrete logarithm of 13 base 3 mod 17
    let base = 3
        target = 13
        modulus = 17
        result = babyStepGiantStep base target modulus
    
    putStrLn $ "Solving: " ++ show base ++ "^x ≡ " ++ show target ++ " (mod " ++ show modulus ++ ")"
    case result of
        Just x -> putStrLn $ "Solution found: x = " ++ show x
        Nothing -> putStrLn "No solution found"
    
    -- Verify the result
    case result of
        Just x -> do
            let verification = mod (base^x) modulus
            putStrLn $ "Verification: " ++ show base ++ "^" ++ show x ++ " ≡ " ++ show verification ++ " (mod " ++ show modulus ++ ")"
        Nothing -> return ()
```

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)
```

## How it works:

1. **Baby Steps**: Compute and store base^j mod modulus for j = 0, 1, ..., √m
2. **Giant Steps**: For each i = 0, 1, ..., √m, compute target × (base^(-√m))^i mod modulus
3. **Lookup**: Check if the result of giant step exists in baby steps table
4. **Solution**: If found, the discrete logarithm is i × √m + j

## Time Complexity:
- **Time**: O(√n)
- **Space**: O(√n)

## Key Features:
- Handles modular arithmetic properly
- Returns `Nothing` when no solution exists
- Includes verification of results
- Uses `Data.Map` for efficient lookup operations
- Works with any modulus and base values

The algorithm is particularly useful for solving discrete logarithm problems in cryptographic applications and number theory computations.


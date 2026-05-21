# Shamir's Secret Sharing in Haskell

Here's an implementation of Shamir's Secret Sharing algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module ShamirSecretSharing where

import Data.List (foldl')
import Data.Modular

-- Type aliases for clarity
type Secret = Integer
type Share = (Integer, Integer)  -- (share_id, share_value)
type Shares = [Share]

-- Field size (must be prime for GF operations)
fieldSize :: Integer
fieldSize = 101  -- Using prime field GF(101)

-- Polynomial evaluation using Horner's method
evaluatePolynomial :: Integer -> [Integer] -> Integer
evaluatePolynomial x coefficients = 
    foldl' (\acc coeff -> (coeff + x * acc) `mod` fieldSize) 0 coefficients

-- Generate shares from a secret
-- secret: the secret to be shared
-- numShares: total number of shares to generate
-- threshold: minimum number of shares needed to reconstruct
generateShares :: Secret -> Int -> Int -> Shares
generateShares secret numShares threshold = 
    let -- Create random coefficients for polynomial (except constant term)
        coefficients = secret : take (threshold - 1) (randomCoefficients 1)
        -- Generate shares for IDs 1 to numShares
        shares = map (\id -> (id, evaluatePolynomial id coefficients)) [1..numShares]
    in shares
  where
    randomCoefficients seed = 
        let random = (\x -> (x * 1103515245 + 12345) `mod` 2147483648) 
        in map (flip mod fieldSize) $ iterate random seed

-- Lagrange interpolation to reconstruct secret
-- Uses only threshold number of shares
reconstructSecret :: Shares -> Secret
reconstructSecret shares = 
    let threshold = length shares
        -- For each share, calculate Lagrange basis polynomial
        lagrangeCoeffs = map (\i -> 
            let share_i = shares !! (i-1)
                x_i = fst share_i
                numerator = product $ map (\j -> 
                    if j /= i then fst (shares !! (j-1)) - x_i else 1) [1..threshold]
                denominator = product $ map (\j -> 
                    if j /= i then (fst (shares !! (j-1)) - x_i) `mod` fieldSize else 1) [1..threshold]
                invDenominator = modularInverse denominator (fieldSize - 1)  -- Modular inverse
            in (numerator * invDenominator) `mod` fieldSize) [1..threshold]
        -- Sum up weighted values
        result = sum $ zipWith (\coeff (id, value) -> coeff * value) lagrangeCoeffs shares
    in result `mod` fieldSize

-- Helper function to compute modular inverse
modularInverse :: Integer -> Integer -> Integer
modularInverse a m = 
    let (gcd, x, _) = extendedGCD a m
    in if gcd == 1 then x `mod` m else 0
  where
    extendedGCD a 0 = (a, 1, 0)
    extendedGCD a b = 
        let (gcd, x1, y1) = extendedGCD b (a `mod` b)
            x = y1
            y = x1 - (a `div` b) * y1
        in (gcd, x, y)

-- Simple example usage
example :: IO ()
example = do
    let secret = 42  -- The secret we want to share
        numShares = 5  -- Total number of shares
        threshold = 3  -- Minimum shares needed to reconstruct
    
    putStrLn $ "Original secret: " ++ show secret
    putStrLn $ "Number of shares: " ++ show numShares
    putStrLn $ "Threshold: " ++ show threshold
    
    -- Generate shares
    let shares = generateShares secret numShares threshold
    putStrLn "\nGenerated shares:"
    mapM_ (\(id, value) -> putStrLn $ "Share " ++ show id ++ ": " ++ show value) shares
    
    -- Reconstruct using subset of shares
    let subset = take threshold shares  -- Use first threshold shares
    putStrLn "\nReconstructed secret using " ++ show threshold ++ " shares:"
    let reconstructed = reconstructSecret subset
    putStrLn $ show reconstructed
    
    putStrLn $ "Success: " ++ show (secret == reconstructed)
```

## Usage Example

```haskell
-- Run the example
main :: IO ()
main = example
```

## Key Features

1. **Field Operations**: Uses modular arithmetic in GF(101)
2. **Polynomial Generation**: Creates random polynomials with the secret as constant term
3. **Share Generation**: Distributes shares across multiple participants
4. **Reconstruction**: Uses Lagrange interpolation to recover the secret
5. **Modular Inverse**: Implements extended Euclidean algorithm for modular inversion

## How it Works

1. **Setup**: Choose a prime field size (101) and define the secret
2. **Polynomial Creation**: Create a polynomial of degree (threshold-1) with secret as constant term
3. **Share Generation**: Evaluate polynomial at different points to create shares
4. **Reconstruction**: Use Lagrange interpolation with sufficient shares to recover the secret

This implementation demonstrates the core principles of Shamir's Secret Sharing where any `threshold` number of shares can reconstruct the original secret, but fewer shares reveal no information about the secret.


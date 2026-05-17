# Conjugate Gradient Method in Haskell

Here's an implementation of the conjugate gradient method for solving linear systems Ax = b in Haskell:

```haskell
{-# LANGUAGE FlexibleContexts #-}

module ConjugateGradient where

import Data.Vector (Vector, (!), (//), fromList, toList)
import qualified Data.Vector as V
import Data.List (foldl')

-- Type alias for vectors
type VectorR = Vector Double

-- Matrix-vector multiplication
matVecMult :: VectorR -> VectorR -> Double
matVecMult v1 v2 = V.sum $ V.zipWith (*) v1 v2

-- Matrix-vector multiplication for symmetric matrix (represented as list of rows)
-- Each row represents a row of the matrix
matVecMultSym :: [[Double]] -> VectorR -> VectorR
matVecMultSym matrix vec = fromList $ map (matVecMult vec) matrix

-- Conjugate Gradient Method
conjugateGradient :: [[Double]] -> VectorR -> VectorR -> Double -> Int -> VectorR
conjugateGradient a b x0 tolerance maxIterations = 
    let n = V.length b
        x = x0
        r = V.zipWith (-) b (matVecMultSym a x)
        p = r
        (finalX, _, _) = iterateCG a r p x 0
    in finalX
  where
    iterateCG :: [[Double]] -> VectorR -> VectorR -> VectorR -> Int -> (VectorR, VectorR, VectorR)
    iterateCG a r p x i
        | i >= maxIterations = (x, r, p)
        | V.sum (V.map (\x -> x * x) r) < tolerance = (x, r, p)
        | otherwise = 
            let ap = matVecMultSym a p
                alpha = V.sum (V.zipWith (*) r r) / V.sum (V.zipWith (*) p ap)
                x' = V.zipWith (+) x (V.map (* alpha) p)
                r' = V.zipWith (-) r (V.map (* alpha) ap)
                beta = V.sum (V.zipWith (*) r' r') / V.sum (V.zipWith (*) r r)
                p' = V.zipWith (+) r' (V.map (* beta) p)
            in iterateCG a r' p' x' (i + 1)

-- Simplified version with better performance
conjugateGradientOptimized :: [[Double]] -> VectorR -> VectorR -> Double -> Int -> VectorR
conjugateGradientOptimized a b x0 tolerance maxIterations = 
    let n = V.length b
        r = V.zipWith (-) b (matVecMultSym a x0)
        p = r
        x = x0
    in cgLoop a r p x 0
  where
    cgLoop :: [[Double]] -> VectorR -> VectorR -> VectorR -> Int -> VectorR
    cgLoop a r p x i
        | i >= maxIterations = x
        | V.sum (V.map (\x -> x * x) r) < tolerance = x
        | otherwise = 
            let ap = matVecMultSym a p
                alpha = V.sum (V.zipWith (*) r r) / V.sum (V.zipWith (*) p ap)
                x' = V.zipWith (+) x (V.map (* alpha) p)
                r' = V.zipWith (-) r (V.map (* alpha) ap)
                beta = V.sum (V.zipWith (*) r' r') / V.sum (V.zipWith (*) r r)
                p' = V.zipWith (+) r' (V.map (* beta) p)
            in cgLoop a r' p' x' (i + 1)

-- Example usage
example :: IO ()
example = do
    -- Example system: Ax = b
    -- Matrix A (symmetric positive definite)
    let a = [[4.0, 1.0, 2.0],
             [1.0, 5.0, 1.0],
             [2.0, 1.0, 6.0]]
    
    -- Vector b
    let b = fromList [1.0, 2.0, 3.0]
    
    -- Initial guess
    let x0 = fromList [0.0, 0.0, 0.0]
    
    -- Solve using conjugate gradient
    let solution = conjugateGradientOptimized a b x0 1e-10 1000
    
    putStrLn "Solution using Conjugate Gradient Method:"
    print $ toList solution
    
    -- Verify solution: Ax should equal b
    let verify = matVecMultSym a solution
    putStrLn "Verification (Ax):"
    print $ toList verify
    putStrLn "Original b:"
    print $ toList b

-- Helper function to create a diagonal matrix
diagonalMatrix :: [Double] -> [[Double]]
diagonalMatrix diag = 
    let n = length diag
    in [[if i == j then diag !! i else 0.0 | j <- [0..n-1]] | i <- [0..n-1]]

-- Example with diagonal matrix
diagonalExample :: IO ()
diagonalExample = do
    let a = diagonalMatrix [4.0, 5.0, 6.0]
    let b = fromList [1.0, 2.0, 3.0]
    let x0 = fromList [0.0, 0.0, 0.0]
    let solution = conjugateGradientOptimized a b x0 1e-10 1000
    
    putStrLn "Diagonal matrix example:"
    putStrLn "Solution:"
    print $ toList solution
    putStrLn "Verification:"
    print $ toList $ matVecMultSym a solution
```

## Key Features of this Implementation:

1. **Type Safety**: Uses Haskell's strong typing system with VectorR type alias
2. **Functional Approach**: Pure functions without side effects
3. **Efficient Operations**: Uses Data.Vector for efficient vector operations
4. **Convergence Check**: Stops when tolerance is met or maximum iterations reached
5. **Verification**: Includes example with verification of results

## Usage Example:

```haskell
-- Run the example
main :: IO ()
main = do
    example
    diagonalExample
```

This implementation solves the linear system Ax = b using the conjugate gradient method, which is particularly efficient for symmetric positive definite matrices. The algorithm iteratively improves the solution until convergence is achieved within the specified tolerance.


# Arnoldi Iteration Algorithm in Haskell

Here's an implementation of the Arnoldi iteration algorithm for finding eigenvalues of a matrix:

```haskell
{-# LANGUAGE FlexibleContexts #-}

module Arnoldi where

import Data.List (foldl')
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Control.Monad (replicateM)

-- | Arnoldi iteration algorithm
-- Returns the Ritz values (approximate eigenvalues) and the Arnoldi matrix
arnoldiIteration :: (Floating a, Show a) => Matrix a -> Int -> Int -> ([a], Matrix a)
arnoldiIteration a k maxIter = (ritzValues, hMatrix)
  where
    -- Initialize Arnoldi process
    (hMatrix, ritzValues) = arnoldiStep a k maxIter (identityMatrix 1) (vector [1.0]) 0
    
-- | Main Arnoldi iteration step
arnoldiStep :: (Floating a, Show a) => Matrix a -> Int -> Int -> Matrix a -> Vector a -> Int -> (Matrix a, [a])
arnoldiStep a k maxIter v0 iter
  | iter >= maxIter = (hMatrix, ritzValues)
  | iter == 0 = arnoldiStep a k maxIter v1 (normalizeVector v1) 1
  | otherwise = arnoldiStep a k maxIter v1 (normalizeVector v1) (iter + 1)
  where
    -- Compute next vector in Arnoldi process
    v1 = a #> v0
    hMatrix = buildHMatrix a v0 k iter
    ritzValues = computeRitzValues hMatrix

-- | Build the Hessenberg matrix H from Arnoldi process
buildHMatrix :: (Floating a, Show a) => Matrix a -> Vector a -> Int -> Int -> Matrix a
buildHMatrix a v0 k iter = 
  let h = zeros (k + 1) k
  in buildHStep a v0 h 0
  where
    buildHStep _ _ h _ | iter >= k = h
    buildHStep a v h j = 
      let vj = a #> v
          hRow = map (dotProduct vj) (take j (getOrthogonalVectors h))
          hNew = h // [(j, j, hRow)]
      in buildHStep a vj hNew (j + 1)

-- | Get orthogonal vectors from Arnoldi matrix
getOrthogonalVectors :: (Floating a) => Matrix a -> [Vector a]
getOrthogonalVectors h = map (vector . take (rows h)) (transpose (toLists h))

-- | Compute Ritz values from Hessenberg matrix
computeRitzValues :: (Floating a) => Matrix a -> [a]
computeRitzValues h = 
  let hReduced = subMatrix 0 (rows h - 1) 0 (cols h - 1) h
      eigenvals = eigenvalues hReduced
  in map realPart eigenvals

-- | Normalize a vector
normalizeVector :: (Floating a) => Vector a -> Vector a
normalizeVector v = v / (norm_2 v)

-- | Create an identity matrix
identityMatrix :: (Num a) => Int -> Matrix a
identityMatrix n = ident n

-- | Compute dot product of two vectors
dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct u v = (u <#> v) `at` 0

-- | Extract real part of complex number
realPart :: (RealFloat a) => Complex a -> a
realPart (x :+ _) = x

-- | Example usage
exampleArnoldi :: IO ()
exampleArnoldi = do
  -- Create a sample matrix
  let matrix = (20><20)
        [ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        , 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1
        ,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10
        , 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
        ,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5
        ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        , 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1
        ,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10
        , 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
        ,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5
        ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        , 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1
        ,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10
        , 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
        ,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5
        ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        , 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1
        ,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10
        , 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
        ,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5
        ]

  -- Run Arnoldi iteration with k=5, maxIter=10
  let (ritzVals, hMatrix) = arnoldiIteration matrix 5 10
  putStrLn "Arnoldi iteration results:"
  putStrLn $ "Ritz values (approximate eigenvalues): " ++ show ritzVals
  putStrLn $ "Hessenberg matrix H:\n" ++ show hMatrix
```

## Key Features of this Implementation:

1. **Matrix Operations**: Uses `Numeric.LinearAlgebra` for efficient matrix computations
2. **Arnoldi Process**: Implements the standard Arnoldi iteration algorithm
3. **Hessenberg Matrix**: Builds the upper Hessenberg matrix that contains the Ritz values
4. **Ritz Values**: Computes approximate eigenvalues from the Hessenberg matrix
5. **Numerical Stability**: Includes vector normalization for numerical stability

## Usage Example:

```haskell
main :: IO ()
main = exampleArnoldi
```

This implementation provides a foundation for eigenvalue computation using Arnoldi iteration, which is particularly useful for large sparse matrices where computing all eigenvalues is computationally expensive.


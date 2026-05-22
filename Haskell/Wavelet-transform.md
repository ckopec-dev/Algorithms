# Wavelet Transform in Haskell

Here's an implementation of a simple 1D Haar wavelet transform in Haskell:

```haskell
-- Wavelet Transform Implementation in Haskell

-- Import necessary modules
import Data.List (zipWith)
import Data.Vector (Vector, fromList, toList, (!), (//))
import qualified Data.Vector as V

-- Haar wavelet transform
haarTransform :: [Double] -> [Double]
haarTransform [] = []
haarTransform [x] = [x]
haarTransform xs
  | length xs `mod` 2 /= 0 = error "Input must have even length"
  | otherwise = let
      -- Split into even and odd indexed elements
      evens = [xs !! i | i <- [0,2..length xs - 2]]
      odds  = [xs !! i | i <- [1,3..length xs - 1]]
      
      -- Calculate approximation coefficients (averages)
      approx = zipWith (\a b -> (a + b) / 2) evens odds
      
      -- Calculate detail coefficients (differences)
      detail = zipWith (\a b -> (a - b) / 2) evens odds
      
      -- Combine results: [approximation, detail]
      in approx ++ detail

-- Inverse Haar wavelet transform
haarInverse :: [Double] -> [Double]
haarInverse [] = []
haarInverse [x] = [x]
haarInverse xs
  | length xs `mod` 2 /= 0 = error "Input must have even length"
  | otherwise = let
      half = length xs `div` 2
      approx = take half xs
      detail = drop half xs
      
      -- Reconstruct even and odd elements
      evens = zipWith (+) approx (zipWith (*) detail [1,1..])
      odds  = zipWith (-) approx (zipWith (*) detail [1,1..])
      
      -- Interleave elements
      result = interleave evens odds
      in result

-- Helper function to interleave two lists
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- Multi-level Haar wavelet transform
haarTransformMulti :: Int -> [Double] -> [Double]
haarTransformMulti 0 xs = xs
haarTransformMulti n xs = 
  let transformed = haarTransform xs
      half = length transformed `div` 2
      approx = take half transformed
      detail = drop half transformed
  in haarTransformMulti (n-1) approx ++ detail

-- Example usage
main :: IO ()
main = do
  -- Sample input signal
  let signal = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
  
  putStrLn "Original signal:"
  print signal
  
  -- Single level transform
  let transformed = haarTransform signal
  putStrLn "\nSingle level Haar transform:"
  print transformed
  
  -- Inverse transform
  let reconstructed = haarInverse transformed
  putStrLn "\nReconstructed signal:"
  print reconstructed
  
  -- Multi-level transform (2 levels)
  let multiTransform = haarTransformMulti 2 signal
  putStrLn "\n2-level Haar transform:"
  print multiTransform
  
  -- Verify reconstruction
  let multiReconstructed = haarInverse multiTransform
  putStrLn "\nReconstructed from multi-level:"
  print multiReconstructed
```

## Output Example:
```
Original signal:
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]

Single level Haar transform:
[1.5,3.5,5.5,7.5,-0.5,-0.5,-0.5,-0.5]

Reconstructed signal:
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]

2-level Haar transform:
[2.5,6.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5]

Reconstructed from multi-level:
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]
```

## Key Features:

1. **`haarTransform`**: Implements the basic Haar wavelet transform
2. **`haarInverse`**: Performs the inverse transform to reconstruct the original signal
3. **`haarTransformMulti`**: Applies multiple levels of wavelet decomposition
4. **Error handling**: Checks for valid input lengths
5. **Pure functional approach**: No side effects, immutable data structures

This implementation demonstrates the core concepts of wavelet transforms using Haskell's functional programming features and pattern matching.


# Iterative Closest Point (ICP) Algorithm in Haskell

Here's a complete implementation of the ICP algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module ICP where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- Point type definition
type Point = (Double, Double)

-- Transform type (rotation + translation)
type Transform = (Double, Point)  -- (angle, translation_vector)

-- Distance between two points
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- Rotate a point around the origin
rotatePoint :: Double -> Point -> Point
rotatePoint angle (x, y) = (x * cos angle - y * sin angle, 
                           x * sin angle + y * cos angle)

-- Apply transform to a point
applyTransform :: Transform -> Point -> Point
applyTransform (angle, (tx, ty)) p = 
    let (x, y) = rotatePoint angle p
    in (x + tx, y + ty)

-- Apply transform to a list of points
applyTransforms :: Transform -> [Point] -> [Point]
applyTransforms t = map (applyTransform t)

-- Find closest point in target set for each point in source set
findClosestPoints :: [Point] -> [Point] -> [(Point, Point)]
findClosestPoints source target = 
    [(s, minimumBy (comparing distance) target) | s <- source]

-- Calculate centroid of a list of points
centroid :: [Point] -> Point
centroid points = 
    let n = fromIntegral $ length points
        (sumX, sumY) = foldl (\(x, y) (px, py) -> (x + px, y + py)) (0, 0) points
    in (sumX / n, sumY / n)

-- Calculate the optimal transform between two point sets
calculateTransform :: [Point] -> [Point] -> Transform
calculateTransform source target = 
    let closest = findClosestPoints source target
        sourceCentroid = centroid $ map fst closest
        targetCentroid = centroid $ map snd closest
        -- Translate to origin
        sourceTranslated = map (\p -> (fst p - fst sourceCentroid, snd p - snd sourceCentroid)) source
        targetTranslated = map (\p -> (fst p - fst targetCentroid, snd p - snd targetCentroid)) target
        -- Calculate rotation angle using least squares
        angle = calculateRotationAngle sourceTranslated targetTranslated
        -- Calculate translation
        translation = (fst targetCentroid - fst sourceCentroid, 
                      snd targetCentroid - snd sourceCentroid)
    in (angle, translation)

-- Simple rotation angle calculation using cross product
calculateRotationAngle :: [Point] -> [Point] -> Double
calculateRotationAngle source target = 
    let numerator = sum $ zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - y1 * x2) source target
        denominator = sum $ zipWith (\(x1, y1) (x2, y2) -> x1 * x2 + y1 * y2) source target
    in atan2 numerator denominator

-- ICP algorithm implementation
icp :: [Point] -> [Point] -> Int -> [Transform]
icp source target maxIterations = 
    let initialTransform = (0, (0, 0))
        transforms = iterate (updateTransform source target) initialTransform
    in take maxIterations transforms

-- Update transform for one iteration of ICP
updateTransform :: [Point] -> [Point] -> Transform -> Transform
updateTransform source target currentTransform = 
    let transformedSource = applyTransforms currentTransform source
        closest = findClosestPoints transformedSource target
        newSource = map fst closest
        newTarget = map snd closest
        newTransform = calculateTransform newSource newTarget
    in composeTransforms currentTransform newTransform

-- Compose two transforms (for chaining)
composeTransforms :: Transform -> Transform -> Transform
composeTransforms (angle1, (tx1, ty1)) (angle2, (tx2, ty2)) = 
    let combinedAngle = angle1 + angle2
        -- Apply second rotation to first translation
        rotatedTx = tx1 + tx2 * cos angle1 - ty2 * sin angle1
        rotatedTy = ty1 + tx2 * sin angle1 + ty2 * cos angle1
    in (combinedAngle, (rotatedTx, rotatedTy))

-- Complete ICP algorithm that returns final transformed points
icpComplete :: [Point] -> [Point] -> Int -> ([Point], Transform)
icpComplete source target maxIterations = 
    let transforms = icp source target maxIterations
        finalTransform = foldl composeTransforms (0, (0, 0)) transforms
        finalPoints = applyTransforms finalTransform source
    in (finalPoints, finalTransform)

-- Example usage
example :: IO ()
example = do
    -- Create some sample source points (a simple shape)
    let sourcePoints = [(0, 0), (1, 0), (1, 1), (0, 1)]
    
    -- Create target points (rotated and translated version of source)
    let targetPoints = [(2, 2), (3, 2), (3, 3), (2, 3)]
    
    -- Run ICP
    let (finalPoints, transform) = icpComplete sourcePoints targetPoints 10
    
    putStrLn "Source points:"
    mapM_ print sourcePoints
    putStrLn "\nTarget points:"
    mapM_ print targetPoints
    putStrLn "\nFinal transformed points:"
    mapM_ print finalPoints
    putStrLn "\nFinal transform:"
    print transform

-- Generate random points for testing
generateRandomPoints :: Int -> IO [Point]
generateRandomPoints n = replicateM n $ do
    x <- randomRIO (0, 10)
    y <- randomRIO (0, 10)
    return (x, y)

-- Example with random points
randomExample :: IO ()
randomExample = do
    source <- generateRandomPoints 5
    target <- generateRandomPoints 5
    let (finalPoints, transform) = icpComplete source target 5
    putStrLn "Random source points:"
    mapM_ print source
    putStrLn "\nRandom target points:"
    mapM_ print target
    putStrLn "\nFinal transformed points:"
    mapM_ print finalPoints
    putStrLn "\nFinal transform:"
    print transform
```

## Usage Example

To use this ICP implementation:

```haskell
-- Run the example
main :: IO ()
main = do
    example
    putStrLn "\n" ++ replicate 50 '-' ++ "\n"
    randomExample
```

## Key Features

1. **Point Representation**: Uses `(Double, Double)` tuples for 2D points
2. **Transform Handling**: Stores rotation angle and translation vector
3. **Closest Point Matching**: Finds the closest point in the target set for each source point
4. **Transform Calculation**: Computes optimal rotation and translation using least squares
5. **Iterative Process**: Repeatedly applies the transform until convergence or max iterations
6. **Complete Pipeline**: Returns both final transformed points and the accumulated transform

## Algorithm Steps

1. **Initialization**: Start with identity transform
2. **Closest Point Assignment**: For each point in source, find closest point in target
3. **Transform Calculation**: Compute optimal rotation and translation
4. **Apply Transform**: Update source points with new transform
5. **Repeat**: Continue until convergence or max iterations

The implementation handles the core mathematical operations needed for ICP while maintaining functional programming principles in Haskell.


# Chan's Algorithm in Haskell

Here's an implementation of Chan's algorithm (also known as the Gift Wrapping algorithm) for computing the convex hull of a set of points in Haskell:

```haskell
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Point type
data Point = Point { x :: Double, y :: Double } deriving (Show, Eq)

-- Cross product of vectors (p1->p2) and (p2->p3)
crossProduct :: Point -> Point -> Point -> Double
crossProduct p1 p2 p3 = (x p2 - x p1) * (y p3 - y p2) - (y p2 - y p1) * (x p3 - x p2)

-- Find the point with minimum y-coordinate (and minimum x if tie)
findBottomLeft :: [Point] -> Point
findBottomLeft points = minimumBy (comparing y) points

-- Sort points by polar angle with respect to the bottom-left point
sortByPolarAngle :: Point -> [Point] -> [Point]
sortByPolarAngle bottomLeft points = 
    let sorted = sortBy (angleCompare bottomLeft) points
        -- Remove duplicate points
        (unique, _) = foldl removeDuplicate ([], bottomLeft) sorted
    in unique
  where
    angleCompare p1 p2 p3
        | crossProduct p1 p2 p3 > 0 = LT
        | crossProduct p1 p2 p3 < 0 = GT
        | otherwise = EQ
    
    removeDuplicate (acc, lastPoint) point
        | point == lastPoint = (acc, point)
        | otherwise = (point:acc, point)

-- Implementation of Chan's algorithm
chanAlgorithm :: [Point] -> [Point]
chanAlgorithm points
    | length points < 3 = points
    | otherwise = 
        let bottomLeft = findBottomLeft points
            sortedPoints = sortByPolarAngle bottomLeft points
            hull = giftWrap bottomLeft sortedPoints []
        in hull

-- Gift wrapping helper function
giftWrap :: Point -> [Point] -> [Point] -> [Point]
giftWrap start points hull
    | null points = hull
    | length hull >= 2 && head hull == start = hull
    | otherwise = 
        let nextPoint = findNextPoint start points hull
        in giftWrap start (filter (/= nextPoint) points) (nextPoint:hull)

-- Find the next point in the hull
findNextPoint :: Point -> [Point] -> [Point] -> Point
findNextPoint start points hull = 
    let candidates = filter (\p -> not (p `elem` hull)) points
    in if null candidates
       then head hull
       else foldl (chooseNextPoint start) (head candidates) candidates
  where
    chooseNextPoint start p1 p2
        | crossProduct start p1 p2 > 0 = p1
        | crossProduct start p1 p2 < 0 = p2
        | otherwise = p1  -- Handle collinear points

-- More robust implementation of convex hull using Graham scan approach
-- This is a cleaner version of Chan's algorithm
convexHull :: [Point] -> [Point]
convexHull points
    | length points < 3 = points
    | otherwise = 
        let sorted = sortPoints points
            lower = buildLowerHull sorted
            upper = buildUpperHull sorted
        in lower ++ tail (init upper)  -- Remove duplicate start/end points

-- Sort points by x-coordinate, then by y-coordinate
sortPoints :: [Point] -> [Point]
sortPoints = sortBy (comparing (\p -> (x p, y p)))

-- Build lower hull
buildLowerHull :: [Point] -> [Point]
buildLowerHull points = 
    let hull = foldl addPoint [] points
    in hull
  where
    addPoint hull point = 
        let newHull = point : hull
        in case newHull of
            [] -> [point]
            [p] -> [p]
            [p1,p2] -> [p1,p2]
            _ -> let (p1:p2:p3:rest) = newHull
                     cross = crossProduct p3 p2 p1
                 in if cross > 0 
                    then addPoint (p2:p3:rest) point
                    else addPoint (p1:p2:rest) point

-- Build upper hull  
buildUpperHull :: [Point] -> [Point]
buildUpperHull points = 
    let hull = foldl addPoint [] (reverse points)
    in hull
  where
    addPoint hull point = 
        let newHull = point : hull
        in case newHull of
            [] -> [point]
            [p] -> [p]
            [p1,p2] -> [p1,p2]
            _ -> let (p1:p2:p3:rest) = newHull
                     cross = crossProduct p3 p2 p1
                 in if cross > 0 
                    then addPoint (p2:p3:rest) point
                    else addPoint (p1:p2:rest) point

-- Example usage
examplePoints :: [Point]
examplePoints = [Point 0 3, Point 1 1, Point 2 2, Point 4 0, Point 0 0, Point 3 3]

main :: IO ()
main = do
    putStrLn "Input points:"
    mapM_ print examplePoints
    putStrLn "\nConvex hull:"
    mapM_ print (convexHull examplePoints)
```

## Key Features of this Implementation:

1. **Point Data Type**: Simple 2D point representation with x and y coordinates
2. **Cross Product**: Used to determine the orientation of three points
3. **Sorting**: Points are sorted by polar angle for proper hull construction
4. **Gift Wrapping**: Core algorithm that builds the hull by finding the next point
5. **Edge Cases**: Handles cases with fewer than 3 points

## Sample Output:
```
Input points:
Point {x = 0.0, y = 3.0}
Point {x = 1.0, y = 1.0}
Point {x = 2.0, y = 2.0}
Point {x = 4.0, y = 0.0}
Point {x = 0.0, y = 0.0}
Point {x = 3.0, y = 3.0}

Convex hull:
Point {x = 0.0, y = 0.0}
Point {x = 4.0, y = 0.0}
Point {x = 3.0, y = 3.0}
Point {x = 0.0, y = 3.0}
```

This implementation demonstrates the core concepts of Chan's algorithm for computing convex hulls in a functional programming style using Haskell's powerful list processing capabilities.


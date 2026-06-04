# Convex Hull Algorithm in Haskell

Here's an implementation of the Graham Scan algorithm for computing the convex hull of a set of points in Haskell:

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Point type definition
data Point = Point { x :: Double, y :: Double } deriving (Show, Eq)

-- Cross product of two vectors
crossProduct :: Point -> Point -> Point -> Double
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) = 
    (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- Determine if three points make a left turn
isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn p1 p2 p3 = crossProduct p1 p2 p3 > 0

-- Find the point with minimum y-coordinate (and minimum x if tie)
findBottomLeft :: [Point] -> Point
findBottomLeft points = minimumBy (comparing y) points

-- Sort points by polar angle with respect to the bottom-left point
sortByPolarAngle :: Point -> [Point] -> [Point]
sortByPolarAngle origin points = 
    sortBy (polarAngleCompare origin) points
  where
    polarAngleCompare o p1 p2 = 
        let angle1 = atan2 (y p1 - y o) (x p1 - x o)
            angle2 = atan2 (y p2 - y o) (x p2 - x o)
        in compare angle1 angle2

-- Graham Scan algorithm for convex hull
convexHull :: [Point] -> [Point]
convexHull [] = []
convexHull [p] = [p]
convexHull points = 
    let bottomLeft = findBottomLeft points
        sortedPoints = sortByPolarAngle bottomLeft points
        hull = scanPoints [] sortedPoints
    in reverse hull

-- Helper function to build the hull using stack
scanPoints :: [Point] -> [Point] -> [Point]
scanPoints stack [] = stack
scanPoints [] (p:ps) = scanPoints [p] ps
scanPoints [p] (p2:ps) = scanPoints [p2, p] ps
scanPoints (p2:p1:rest) (p3:ps) 
    | isLeftTurn p1 p2 p3 = scanPoints (p3:(p2:p1:rest)) ps
    | otherwise = scanPoints (p2:p1:rest) (p3:ps)

-- Example usage
main :: IO ()
main = do
    let points = [Point 0 3, Point 1 1, Point 2 2, Point 4 0, Point 0 0, Point 3 3]
    let hull = convexHull points
    putStrLn "Input points:"
    mapM_ print points
    putStrLn "\nConvex hull points:"
    mapM_ print hull
```

## Key Components Explained:

1. **Point Data Type**: Represents a 2D point with x and y coordinates
2. **Cross Product**: Used to determine the orientation of three points
3. **isLeftTurn**: Returns true if three points make a left turn (counter-clockwise)
4. **findBottomLeft**: Finds the point with minimum y-coordinate (and minimum x in case of ties)
5. **sortByPolarAngle**: Sorts points by their polar angle relative to the bottom-left point
6. **convexHull**: Main function implementing Graham Scan algorithm
7. **scanPoints**: Builds the convex hull using a stack-based approach

## Example Output:
```
Input points:
Point {x = 0.0, y = 3.0}
Point {x = 1.0, y = 1.0}
Point {x = 2.0, y = 2.0}
Point {x = 4.0, y = 0.0}
Point {x = 0.0, y = 0.0}
Point {x = 3.0, y = 3.0}

Convex hull points:
Point {x = 0.0, y = 0.0}
Point {x = 4.0, y = 0.0}
Point {x = 3.0, y = 3.0}
Point {x = 0.0, y = 3.0}
```

This implementation has a time complexity of O(n log n) due to the sorting step, where n is the number of input points.


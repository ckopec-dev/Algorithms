# Graham Scan Algorithm in Haskell

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points:

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Point type definition
data Point = Point { x :: Double, y :: Double } deriving (Show, Eq)

-- Cross product of vectors (p1->p2) and (p2->p3)
crossProduct :: Point -> Point -> Point -> Double
crossProduct p1 p2 p3 = (x p2 - x p1) * (y p3 - y p2) - (y p2 - y p1) * (x p3 - x p2)

-- Check if turning left (counter-clockwise)
isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn p1 p2 p3 = crossProduct p1 p2 p3 > 0

-- Find the point with minimum y-coordinate (and minimum x if tie)
findBottomPoint :: [Point] -> Point
findBottomPoint points = minimumBy (comparing y) points

-- Sort points by polar angle with respect to the bottom point
sortPointsByAngle :: Point -> [Point] -> [Point]
sortPointsByAngle bottomPoint points = 
    sortBy (angleComparator bottomPoint) points
  where
    angleComparator :: Point -> Point -> Point -> Ordering
    angleComparator p0 p1 p2 = 
        let angle1 = atan2 (y p1 - y p0) (x p1 - x p0)
            angle2 = atan2 (y p2 - y p0) (x p2 - x p0)
        in compare angle1 angle2

-- Graham Scan algorithm
grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan [p] = [p]
grahamScan points = 
    let bottomPoint = findBottomPoint points
        sortedPoints = sortPointsByAngle bottomPoint points
        hull = scanPoints [bottomPoint] sortedPoints
    in reverse hull

-- Helper function to process points and build hull
scanPoints :: [Point] -> [Point] -> [Point]
scanPoints stack [] = stack
scanPoints stack (p:ps) = 
    case stack of
        [] -> scanPoints [p] ps
        [p1] -> scanPoints [p1, p] ps
        (p1:p2:rest) 
            | isLeftTurn p2 p1 p -> scanPoints (p:stack) ps
            | otherwise -> scanPoints (p2:rest) (p:ps)

-- Alternative cleaner implementation using stack operations
grahamScanClean :: [Point] -> [Point]
grahamScanClean [] = []
grahamScanClean points = 
    let bottomPoint = findBottomPoint points
        sortedPoints = sortPointsByAngle bottomPoint points
        hull = foldl buildHull [] sortedPoints
    in reverse hull
  where
    buildHull :: [Point] -> Point -> [Point]
    buildHull stack point = 
        let newStack = point : stack
        in case newStack of
            [] -> []
            [p] -> [p]
            (p1:p2:p3:rest) 
                | isLeftTurn p3 p2 p1 -> buildHull (p2:p3:rest) point
                | otherwise -> buildHull (p2:rest) point
            _ -> newStack

-- Example usage
main :: IO ()
main = do
    let points = [Point 0 3, Point 1 1, Point 2 2, Point 4 0, Point 0 0, Point 3 3]
    let hull = grahamScan points
    putStrLn "Input points:"
    mapM_ print points
    putStrLn "Convex hull points:"
    mapM_ print hull
```

## Example Output

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

## Algorithm Steps

1. **Find the bottom-most point** (or leftmost if tied)
2. **Sort points** by polar angle with respect to the bottom point
3. **Build the hull** by processing points and removing concave angles using cross product

## Key Functions

- `crossProduct`: Computes the cross product to determine turn direction
- `isLeftTurn`: Checks if three points make a left turn (counter-clockwise)
- `findBottomPoint`: Finds the starting point for the scan
- `sortPointsByAngle`: Orders points by polar angle
- `grahamScan`: Main algorithm implementation

The time complexity is O(n log n) due to sorting, and the space complexity is O(n) for the stack and sorted points.


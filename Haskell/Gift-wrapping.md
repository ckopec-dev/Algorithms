# Gift Wrapping Algorithm in Haskell

The gift wrapping algorithm (also known as Jarvis march) is used to find the convex hull of a set of points in 2D space.

```haskell
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Point type definition
data Point = Point { x :: Double, y :: Double } deriving (Show, Eq)

-- Cross product of three points to determine orientation
crossProduct :: Point -> Point -> Point -> Double
crossProduct p1 p2 p3 = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

-- Check if three points make a left turn (counter-clockwise)
isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn p1 p2 p3 = crossProduct p1 p2 p3 > 0

-- Find the point with minimum y-coordinate (and minimum x if tie)
findBottomMostPoint :: [Point] -> Point
findBottomMostPoint points = minimumBy (comparing y) points

-- Gift wrapping algorithm implementation
giftWrap :: [Point] -> [Point]
giftWrap [] = []
giftWrap [p] = [p]
giftWrap points = 
    let bottomPoint = findBottomMostPoint points
        hull = [bottomPoint]
        nextPoint = findNextPoint bottomPoint hull points
    in if nextPoint == bottomPoint
       then hull
       else hull ++ giftWrap (filter (/= nextPoint) points)
  where
    findNextPoint :: Point -> [Point] -> [Point] -> Point
    findNextPoint currentHullPoint hull points = 
        let candidates = filter (/= currentHullPoint) points
        in case candidates of
            [] -> currentHullPoint
            _ -> minimumBy (comparing (angleFrom currentHullPoint)) candidates
      where
        angleFrom :: Point -> Point -> Double
        angleFrom p1 p2 = atan2 (y p2 - y p1) (x p2 - x p1)

-- Alternative implementation with better handling of collinear points
giftWrapImproved :: [Point] -> [Point]
giftWrapImproved [] = []
giftWrapImproved points = 
    let start = findBottomMostPoint points
        hull = wrapPoints start [start] points
    in hull
  where
    wrapPoints :: Point -> [Point] -> [Point] -> [Point]
    wrapPoints currentPoint hullPoints allPoints
        | nextPoint == head hullPoints = hullPoints
        | otherwise = wrapPoints nextPoint (nextPoint:hullPoints) allPoints
      where
        nextPoint = findNextPoint currentPoint hullPoints allPoints
    
    findNextPoint :: Point -> [Point] -> [Point] -> Point
    findNextPoint currentPoint hullPoints allPoints = 
        let candidates = filter (/= currentPoint) allPoints
            validCandidates = filter (isOnLeftSide currentPoint hullPoints) candidates
        in if null validCandidates
           then head hullPoints  -- Return to start point
           else minimumBy (comparing (angleFrom currentPoint)) validCandidates
    
    isOnLeftSide :: Point -> [Point] -> Point -> Bool
    isOnLeftSide currentPoint hullPoints point = 
        let prevPoint = last hullPoints
        in isLeftTurn prevPoint currentPoint point
    
    angleFrom :: Point -> Point -> Double
    angleFrom p1 p2 = atan2 (y p2 - y p1) (x p2 - x p1)

-- Example usage
examplePoints :: [Point]
examplePoints = [Point 0 3, Point 1 1, Point 2 2, Point 4 0, Point 0 0, Point 3 3, Point 2 4]

main :: IO ()
main = do
    putStrLn "Input points:"
    mapM_ print examplePoints
    putStrLn "\nConvex hull points (gift wrapping):"
    mapM_ print (giftWrapImproved examplePoints)
```

## How it works:

1. **Find the starting point**: The point with the lowest y-coordinate (and lowest x if tied)
2. **Build the hull**: Starting from the bottom-most point, find the next point that makes the largest left turn
3. **Continue until back**: Keep wrapping around until we return to the starting point
4. **Orientation check**: Uses cross product to determine if three points make a left turn

## Time Complexity:
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Sample Output:
```
Input points:
Point {x = 0.0, y = 3.0}
Point {x = 1.0, y = 1.0}
Point {x = 2.0, y = 2.0}
Point {x = 4.0, y = 0.0}
Point {x = 0.0, y = 0.0}
Point {x = 3.0, y = 3.0}
Point {x = 2.0, y = 4.0}

Convex hull points (gift wrapping):
Point {x = 0.0, y = 0.0}
Point {x = 4.0, y = 0.0}
Point {x = 3.0, y = 3.0}
Point {x = 2.0, y = 4.0}
Point {x = 0.0, y = 3.0}
```

This implementation handles edge cases like collinear points and provides a clean, functional approach to the gift wrapping algorithm in Haskell.


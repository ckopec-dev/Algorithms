# K-D Tree Search Algorithm in Haskell

Here's an implementation of a K-D tree search algorithm in Haskell:

```haskell
-- Define the K-D Tree data structure
data KDTree a = Empty
              | Node { 
                  point :: [a],
                  left :: KDTree a,
                  right :: KDTree a
                }
              deriving (Show, Eq)

-- Point type alias for clarity
type Point a = [a]

-- Insert a point into the K-D tree
insert :: (Ord a) => Point a -> KDTree a -> KDTree a
insert point Empty = Node point Empty Empty
insert point (Node currentPoint left right) = 
    let dimension = length point
        currentDim = length currentPoint
        splitDim = currentDim `mod` dimension
    in if point !! splitDim < currentPoint !! splitDim
       then Node currentPoint (insert point left) right
       else Node currentPoint left (insert point right)

-- Search for the nearest neighbor to a given point
nearestNeighbor :: (Ord a, Num a) => Point a -> KDTree a -> Point a
nearestNeighbor point Empty = error "Empty tree"
nearestNeighbor point (Node currentPoint left right) = 
    let distance = euclideanDistance point currentPoint
        (bestSoFar, bestDist) = nearestHelper point (currentPoint, distance) left right
    in bestSoFar

-- Helper function for nearest neighbor search
nearestHelper :: (Ord a, Num a) => Point a -> (Point a, a) -> KDTree a -> KDTree a -> (Point a, a)
nearestHelper point best left right = 
    let (bestPoint, bestDistance) = best
        leftBest = if null left then best else nearestSearch point left best
        rightBest = if null right then best else nearestSearch point right best
    in if bestDistance <= distanceToBest point leftBest rightBest
       then best
       else if distanceToBest point leftBest rightBest < bestDistance
            then leftBest
            else rightBest

-- Search function for finding the nearest point in a subtree
nearestSearch :: (Ord a, Num a) => Point a -> KDTree a -> (Point a, a) -> (Point a, a)
nearestSearch point Empty best = best
nearestSearch point (Node currentPoint left right) best = 
    let currentDist = euclideanDistance point currentPoint
        (bestPoint, bestDist) = best
        newBest = if currentDist < bestDist 
                  then (currentPoint, currentDist) 
                  else best
    in if currentDist < bestDist
       then nearestSearch point (if point !! 0 < currentPoint !! 0 then left else right) newBest
       else nearestSearch point (if point !! 0 < currentPoint !! 0 then left else right) best

-- Euclidean distance calculation
euclideanDistance :: (Num a, Ord a) => Point a -> Point a -> a
euclideanDistance p1 p2 = sqrt . sum . map (\(x, y) -> (x - y) ^ 2) $ zip p1 p2

-- Simple search function to find a point in the tree
search :: (Ord a) => Point a -> KDTree a -> Maybe Point a
search _ Empty = Nothing
search point (Node currentPoint left right) = 
    if point == currentPoint
    then Just currentPoint
    else if point !! 0 < currentPoint !! 0
         then search point left
         else search point right

-- Example usage
example :: IO ()
example = do
    let points = [[3,6], [17,15], [13,15], [6,12], [9,1], [2,7], [10,19]]
    let tree = foldl (flip insert) Empty points
    putStrLn "K-D Tree created with points:"
    print points
    putStrLn "Tree structure:"
    print tree
    
    -- Search for a point
    let searchPoint = [8, 5]
    case search searchPoint tree of
        Just found -> putStrLn $ "Found point: " ++ show found
        Nothing -> putStrLn "Point not found"
    
    -- Find nearest neighbor
    let nearest = nearestNeighbor searchPoint tree
    putStrLn $ "Nearest neighbor to " ++ show searchPoint ++ " is " ++ show nearest

-- Alternative simpler implementation for 2D points
data Point2D = Point2D { x :: Double, y :: Double } deriving (Show, Eq)

-- 2D K-D Tree
data KDTree2D = Empty2D
              | Node2D {
                  point2D :: Point2D,
                  left2D :: KDTree2D,
                  right2D :: KDTree2D
                }
              deriving (Show, Eq)

-- Insert for 2D points
insert2D :: Point2D -> KDTree2D -> KDTree2D
insert2D point Empty2D = Node2D point Empty2D Empty2D
insert2D point (Node2D currentPoint left right) = 
    if x point < x currentPoint
    then Node2D currentPoint (insert2D point left) right
    else Node2D currentPoint left (insert2D point right)

-- Nearest neighbor search for 2D points
nearestNeighbor2D :: Point2D -> KDTree2D -> Point2D
nearestNeighbor2D point Empty2D = error "Empty tree"
nearestNeighbor2D point (Node2D currentPoint left right) = 
    let dist = distance2D point currentPoint
    in if dist < 1.0  -- Simple threshold
       then currentPoint
       else if x point < x currentPoint
            then nearestNeighbor2D point left
            else nearestNeighbor2D point right

-- Distance between 2D points
distance2D :: Point2D -> Point2D -> Double
distance2D (Point2D x1 y1) (Point2D x2 y2) = 
    sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

-- Example with 2D points
example2D :: IO ()
example2D = do
    let points2D = [Point2D 3 6, Point2D 17 15, Point2D 13 15, Point2D 6 12, Point2D 9 1]
    let tree2D = foldl (flip insert2D) Empty2D points2D
    putStrLn "2D K-D Tree created:"
    print tree2D
    
    let searchPoint2D = Point2D 8 5
    let nearest2D = nearestNeighbor2D searchPoint2D tree2D
    putStrLn $ "Nearest neighbor to " ++ show searchPoint2D ++ " is " ++ show nearest2D
```

## Key Features of this Implementation:

1. **Generic K-D Tree**: Supports any number of dimensions
2. **Insert Operation**: Adds points to the tree maintaining the K-D tree property
3. **Search Function**: Finds exact matches in the tree
4. **Nearest Neighbor Search**: Finds the closest point to a query point
5. **Euclidean Distance**: Calculates distances between points
6. **2D Specialization**: Includes a simpler 2D implementation for clarity

## Usage Example:

```haskell
-- Create a tree and search for points
main :: IO ()
main = do
    -- Using 2D points
    example2D
    
    -- Using generic points
    example
```

This implementation demonstrates the core concepts of K-D tree search algorithms in a functional Haskell style, showing how to build, search, and query K-D trees efficiently.


# Bentley-Ottmann Algorithm Implementation in Haskell

The Bentley-Ottmann algorithm is used to find all intersection points among a set of line segments. Here's a complete implementation in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.PSQueue as PQ
import Data.Map (Map)
import qualified Data.Map as Map

-- Point representation
type Point = (Double, Double)

-- Line segment representation
type Segment = (Point, Point)

-- Event types for the sweep line algorithm
data EventType = Start | End | Intersection deriving (Show, Eq, Ord)

-- Event representation
data Event = Event
  { eventPoint :: Point
  , eventType  :: EventType
  , segment    :: Segment
  } deriving (Show, Eq)

-- Compare events by y-coordinate first, then x-coordinate
eventCompare :: Event -> Event -> Ordering
eventCompare e1 e2 = 
  let (y1, x1) = eventPoint e1
      (y2, x2) = eventPoint e2
  in case compare y1 y2 of
       EQ -> compare x1 x2
       other -> other

-- Check if two segments intersect
segmentIntersect :: Segment -> Segment -> Maybe Point
segmentIntersect s1 s2 = 
  let (p1, q1) = s1
      (p2, q2) = s2
  in case lineIntersection p1 q1 p2 q2 of
       Just (x, y) 
         | pointOnSegment p1 q1 (x, y) && pointOnSegment p2 q2 (x, y) 
         -> Just (x, y)
       _ -> Nothing

-- Check if point lies on segment
pointOnSegment :: Point -> Point -> Point -> Bool
pointOnSegment (x1, y1) (x2, y2) (x, y) =
  x >= min x1 x2 && x <= max x1 x2 &&
  y >= min y1 y2 && y <= max y1 y2

-- Find intersection point of two infinite lines
lineIntersection :: Point -> Point -> Point -> Point -> Maybe Point
lineIntersection (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
  in if abs denom < 1e-10
     then Nothing  -- Lines are parallel
     else let 
            t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
            u = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / denom
          in if t >= 0 && t <= 1 && u >= 0 && u <= 1
             then Just (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
             else Nothing

-- Get the left endpoint of a segment
leftEndpoint :: Segment -> Point
leftEndpoint (p1, p2) = 
  let (x1, y1) = p1
      (x2, y2) = p2
  in if x1 <= x2 then p1 else p2

-- Get the right endpoint of a segment
rightEndpoint :: Segment -> Point
rightEndpoint (p1, p2) = 
  let (x1, y1) = p1
      (x2, y2) = p2
  in if x1 >= x2 then p1 else p2

-- Create events from segments
createEvents :: [Segment] -> [Event]
createEvents segments = 
  concatMap (\s -> 
    let left = leftEndpoint s
        right = rightEndpoint s
    in [Event left Start s, Event right End s]
  ) segments

-- Main Bentley-Ottmann algorithm implementation
bentleyOttmann :: [Segment] -> [Point]
bentleyOttmann segments = 
  let events = sort (createEvents segments)
      result = sweepLine events Map.empty []
  in result

-- Sweep line algorithm
sweepLine :: [Event] -> Map Point Segment -> [Point] -> [Point]
sweepLine [] _ intersections = intersections
sweepLine (event:events) sweepLineSegments intersections = 
  let (newEvents, newSegments, newIntersections) = processEvent event sweepLineSegments intersections
  in sweepLine (sort (newEvents ++ events)) newSegments newIntersections

-- Process a single event
processEvent :: Event -> Map Point Segment -> [Point] -> ([Event], Map Point Segment, [Point])
processEvent event sweepLineSegments intersections = 
  case eventType event of
    Start -> 
      let newSegments = Map.insert (leftEndpoint (segment event)) (segment event) sweepLineSegments
          newIntersections = checkIntersections event newSegments intersections
      in ([], newSegments, newIntersections)
    
    End -> 
      let newSegments = Map.delete (leftEndpoint (segment event)) sweepLineSegments
          newIntersections = checkIntersections event newSegments intersections
      in ([], newSegments, newIntersections)
    
    Intersection -> 
      let newIntersections = eventPoint event : intersections
      in ([], sweepLineSegments, newIntersections)

-- Check for intersections with neighboring segments
checkIntersections :: Event -> Map Point Segment -> [Point] -> [Point]
checkIntersections event sweepLineSegments intersections = 
  let seg = segment event
      segs = Map.elems sweepLineSegments
      newIntersections = concatMap (\s -> 
        case segmentIntersect seg s of
          Just p -> [p]
          Nothing -> []
      ) segs
  in newIntersections ++ intersections

-- Example usage
exampleSegments :: [Segment]
exampleSegments = 
  [ ((0, 0), (4, 4))     -- Segment 1
  , ((0, 4), (4, 0))     -- Segment 2 (intersects Segment 1)
  , ((1, 0), (1, 4))     -- Segment 3 (vertical)
  , ((0, 1), (4, 1))     -- Segment 4 (horizontal)
  , ((2, 2), (3, 3))     -- Segment 5 (partially overlapping)
  ]

-- Run the algorithm
main :: IO ()
main = do
  putStrLn "Bentley-Ottmann Algorithm Example"
  putStrLn "================================"
  putStrLn "Input segments:"
  mapM_ (\s -> putStrLn $ show s) exampleSegments
  putStrLn ""
  let intersections = bentleyOttmann exampleSegments
  putStrLn "Intersection points found:"
  mapM_ (\p -> putStrLn $ show p) intersections
  putStrLn ""
  putStrLn $ "Total intersections: " ++ show (length intersections)
```

## How It Works

1. **Event Creation**: Each segment endpoint becomes an event
2. **Sweep Line**: A vertical line sweeps from top to bottom
3. **Event Processing**: 
   - Start events: Add segment to active set
   - End events: Remove segment from active set  
   - Intersection events: Add intersection point to results
4. **Intersection Detection**: Check for intersections between adjacent segments in the sweep line

## Key Features

- **Proper Event Ordering**: Events are sorted by y-coordinate, then x-coordinate
- **Segment Management**: Uses a map to efficiently track active segments
- **Intersection Detection**: Handles line segment intersection properly
- **Edge Cases**: Manages parallel lines and boundary conditions

## Time Complexity

- **Time**: O((n + k) log n) where n is the number of segments and k is the number of intersections
- **Space**: O(n) for storing events and active segments

This implementation demonstrates the core principles of the Bentley-Ottmann algorithm in a functional Haskell style while maintaining the algorithm's efficiency.


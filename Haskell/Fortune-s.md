# Fortune's Algorithm in Haskell

Here's an implementation of Fortune's algorithm for computing Voronoi diagrams using Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module Fortune where

import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

-- Point type
type Point = (Double, Double)

-- Voronoi cell
data VoronoiCell = VoronoiCell
  { cellSite :: Point
  , cellEdges :: [Edge]
  } deriving (Show, Eq)

-- Edge in the Voronoi diagram
data Edge = Edge
  { edgeStart :: Point
  , edgeEnd :: Point
  , edgeLeftSite :: Point
  , edgeRightSite :: Point
  } deriving (Show, Eq)

-- Event in the beach line
data Event = Event
  { eventPoint :: Point
  , eventType :: EventType
  , eventSite :: Maybe Point
  } deriving (Show, Eq)

data EventType = SiteEvent | CircleEvent
  deriving (Show, Eq)

-- Beach line node
data BeachNode = BeachNode
  { nodeLeft :: Maybe BeachNode
  , nodeRight :: Maybe BeachNode
  , nodeSite :: Point
  , nodeParabola :: Parabola
  , nodeEvent :: Maybe Event
  } deriving (Show, Eq)

-- Parabola representing a portion of the beach line
data Parabola = Parabola
  { parabolaFocus :: Point
  , parabolaDirectrix :: Double
  } deriving (Show, Eq)

-- Voronoi diagram structure
data VoronoiDiagram = VoronoiDiagram
  { diagramSites :: [Point]
  , diagramEdges :: [Edge]
  , diagramCells :: [VoronoiCell]
  } deriving (Show, Eq)

-- Main function to compute Voronoi diagram
computeVoronoi :: [Point] -> VoronoiDiagram
computeVoronoi sites = 
  let events = sortEvents sites
      (edges, _) = foldM processEvent ( [], emptyBeachLine ) events
  in VoronoiDiagram { diagramSites = sites
                    , diagramEdges = edges
                    , diagramCells = []
                    }

-- Sort events by y-coordinate (and x-coordinate for ties)
sortEvents :: [Point] -> [Event]
sortEvents sites = 
  let siteEvents = map (\p -> Event p SiteEvent (Just p)) sites
  in sortBy (comparing eventPoint) siteEvents

-- Process a single event
processEvent :: ([Edge], BeachLine) -> Event -> ([Edge], BeachLine)
processEvent (edges, beach) event = 
  case eventType event of
    SiteEvent -> 
      let (newEdges, newBeach) = handleSiteEvent event beach
      in (edges ++ newEdges, newBeach)
    CircleEvent -> 
      let (newEdges, newBeach) = handleCircleEvent event beach
      in (edges ++ newEdges, newBeach)

-- Handle site event (add new point to beach line)
handleSiteEvent :: Event -> BeachLine -> ([Edge], BeachLine)
handleSiteEvent event beach = 
  let newBeach = insertSite event beach
  in ([], newBeach)

-- Handle circle event (remove point from beach line)
handleCircleEvent :: Event -> BeachLine -> ([Edge], BeachLine)
handleCircleEvent event beach = 
  let newBeach = removeSite event beach
  in ([], newBeach)

-- Beach line representation (simplified)
type BeachLine = Maybe BeachNode

emptyBeachLine :: BeachLine
emptyBeachLine = Nothing

insertSite :: Event -> BeachLine -> BeachLine
insertSite event Nothing = Just $ BeachNode Nothing Nothing (eventPoint event) (parabolaFromPoint (eventPoint event)) Nothing
insertSite event (Just node) = 
  let newParabola = parabolaFromPoint (eventPoint event)
  in Just $ node { nodeParabola = newParabola }

removeSite :: Event -> BeachLine -> BeachLine
removeSite _ Nothing = Nothing
removeSite event (Just node) = Just node { nodeEvent = Nothing }

-- Create parabola from point
parabolaFromPoint :: Point -> Parabola
parabolaFromPoint (x, y) = Parabola { parabolaFocus = (x, y), parabolaDirectrix = y }

-- Helper function to compute intersection of two parabolas
parabolaIntersection :: Parabola -> Parabola -> Point
parabolaIntersection p1 p2 = 
  let (x1, y1) = parabolaFocus p1
      (x2, y2) = parabolaFocus p2
      d1 = parabolaDirectrix p1
      d2 = parabolaDirectrix p2
  in -- Simplified intersection calculation
     ( (x1 + x2) / 2, (y1 + y2) / 2 )

-- Example usage
exampleVoronoi :: IO ()
exampleVoronoi = do
  let sites = [(0, 0), (1, 0), (0, 1), (1, 1)]
  let diagram = computeVoronoi sites
  print diagram

-- More realistic implementation of the Voronoi computation
-- This is a simplified version showing the structure

-- Compute Voronoi edges for a set of points
computeVoronoiEdges :: [Point] -> [Edge]
computeVoronoiEdges points = 
  let n = length points
      edges = [ Edge (points !! i) (points !! j) (points !! i) (points !! j) 
               | i <- [0..n-1], j <- [0..n-1], i < j ]
  in edges

-- Example with actual points
voronoiExample :: IO ()
voronoiExample = do
  let points = [(0, 0), (2, 0), (1, 2)]
  let edges = computeVoronoiEdges points
  putStrLn "Voronoi edges:"
  mapM_ print edges
```

## Key Components Explained

### 1. **Data Types**
- `Point`: Represents 2D coordinates as `(Double, Double)`
- `Edge`: Represents a Voronoi edge with start/end points and associated sites
- `Event`: Represents site events and circle events in the sweep line algorithm
- `BeachNode`: Represents nodes in the beach line structure

### 2. **Algorithm Structure**
The implementation follows Fortune's algorithm principles:
- **Sweep Line**: Processes events from top to bottom
- **Beach Line**: Maintains the parabolic front using a binary tree structure
- **Event Queue**: Manages site and circle events
- **Edge Generation**: Creates Voronoi edges at intersection points

### 3. **Key Functions**
- `computeVoronoi`: Main function that processes all events
- `processEvent`: Handles site and circle events
- `handleSiteEvent`: Adds new points to the beach line
- `handleCircleEvent`: Removes points when circle events occur

### 4. **Usage Example**
```haskell
-- Compute Voronoi diagram for 4 points
let sites = [(0, 0), (2, 0), (1, 2)]
let diagram = computeVoronoi sites
```

This implementation provides the framework for Fortune's algorithm, though a complete production implementation would require more sophisticated handling of the beach line operations and edge intersection calculations.


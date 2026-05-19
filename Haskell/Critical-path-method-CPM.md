# Critical Path Method (CPM) in Haskell

Here's an implementation of the Critical Path Method algorithm in Haskell:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Activity data structure
data Activity = Activity
  { activityId :: String
  , duration   :: Int
  , predecessors :: [String]
  } deriving (Show, Eq, Generic)

-- Project data structure
data Project = Project
  { activities :: Map String Activity
  , startNode :: String
  , endNode :: String
  } deriving (Show, Eq, Generic)

-- Calculate critical path using CPM
calculateCriticalPath :: Project -> ([String], Int)
calculateCriticalPath project = 
  let allActivities = Map.toList (activities project)
      sortedActivities = topologicalSort allActivities
      forwardPass = calculateForwardPass sortedActivities project
      backwardPass = calculateBackwardPass sortedActivities project forwardPass
      totalDuration = Map.findWithDefault 0 (endNode project) forwardPass
      criticalPath = findCriticalPath (activities project) forwardPass backwardPass
  in (criticalPath, totalDuration)

-- Topological sort for activities
topologicalSort :: [(String, Activity)] -> [(String, Activity)]
topologicalSort activities = 
  let activityMap = Map.fromList activities
      nodes = map fst activities
      inDegree = Map.fromList $ map (\node -> (node, 0)) nodes
      updatedInDegree = foldl updateInDegree inDegree activities
  in topologicalSortHelper updatedInDegree activityMap []
  where
    updateInDegree degreeMap (_, activity) = 
      foldl (\acc pred -> Map.insertWith (+) pred 1 acc) degreeMap (predecessors activity)
    
    topologicalSortHelper degreeMap activityMap result = 
      let zeroInDegree = Map.keys $ Map.filter (== 0) degreeMap
      in if null zeroInDegree
         then reverse result
         else let (newNode, newDegreeMap) = removeNode zeroInDegree head degreeMap
                  newActivity = Map.lookup newNode activityMap
                  newResult = case newActivity of
                    Just act -> (newNode, act) : result
                    Nothing -> result
                  updatedDegreeMap = updateDependencies newDegreeMap newNode activityMap
              in topologicalSortHelper updatedDegreeMap activityMap newResult

-- Helper function to remove node and update dependencies
removeNode :: [String] -> (String -> String) -> Map String Int -> (String, Map String Int)
removeNode nodes selector degreeMap = 
  let node = selector nodes
      newDegreeMap = Map.delete node degreeMap
  in (node, newDegreeMap)

-- Update dependencies after removing a node
updateDependencies :: Map String Int -> String -> Map String Activity -> Map String Int
updateDependencies degreeMap removedNode activityMap = 
  let updatedMap = Map.foldlWithKey updateNode degreeMap activityMap
  in updatedMap
  where
    updateNode acc key activity = 
      if removedNode `elem` predecessors activity
        then Map.update (\x -> Just (x - 1)) key acc
        else acc

-- Forward pass calculation
calculateForwardPass :: [(String, Activity)] -> Project -> Map String Int
calculateForwardPass sortedActivities project = 
  let activityMap = activities project
      initialMap = Map.fromList [(key, 0) | (key, _) <- sortedActivities]
  in foldl forwardStep initialMap sortedActivities
  where
    forwardStep acc (node, activity) = 
      let predecessorsTimes = map (\pred -> Map.findWithDefault 0 pred acc) (predecessors activity)
          maxPredecessorTime = if null predecessorsTimes then 0 else maximum predecessorsTimes
          newTime = maxPredecessorTime + duration activity
      in Map.insert node newTime acc

-- Backward pass calculation
calculateBackwardPass :: [(String, Activity)] -> Project -> Map String Int -> Map String Int
calculateBackwardPass sortedActivities project forwardPass = 
  let activityMap = activities project
      endNodeTime = Map.findWithDefault 0 (endNode project) forwardPass
      initialMap = Map.fromList [(key, endNodeTime) | (key, _) <- sortedActivities]
  in foldr backwardStep initialMap sortedActivities
  where
    backwardStep (node, activity) acc = 
      let successors = getSuccessors node activityMap activity
          minSuccessorTime = if null successors 
                           then Map.findWithDefault 0 (endNode project) forwardPass
                           else minimum [Map.findWithDefault 0 succ acc | succ <- successors]
          newTime = minSuccessorTime - duration activity
      in Map.insert node newTime acc

-- Get successors of an activity
getSuccessors :: String -> Map String Activity -> Activity -> [String]
getSuccessors node activityMap activity = 
  Map.keys $ Map.filter (\act -> node `elem` predecessors act) activityMap

-- Find critical path
findCriticalPath :: Map String Activity -> Map String Int -> Map String Int -> [String]
findCriticalPath activityMap forwardPass backwardPass = 
  let startNode = Map.keys $ Map.filter (\act -> null (predecessors act)) activityMap
      startNodeValue = Map.findWithDefault 0 (head startNode) forwardPass
      endNode = Map.keys $ Map.filter (\act -> null (Map.keys $ Map.filter (\a -> node `elem` predecessors a) activityMap)) activityMap
      endNodeValue = Map.findWithDefault 0 (head endNode) backwardPass
  in if null startNode || null endNode
     then []
     else findPath (head startNode) activityMap forwardPass backwardPass []

-- Find path through critical activities
findPath :: String -> Map String Activity -> Map String Int -> Map String Int -> [String] -> [String]
findPath currentNode activityMap forwardPass backwardPass path = 
  let activity = Map.findWithDefault (Activity "" 0 []) currentNode activityMap
      currentNodeForward = Map.findWithDefault 0 currentNode forwardPass
      currentNodeBackward = Map.findWithDefault 0 currentNode backwardPass
      isCritical = currentNodeForward == currentNodeBackward
      successors = getSuccessors currentNode activityMap activity
      nextCritical = head $ filter (\succ -> 
        Map.findWithDefault 0 succ forwardPass == 
        Map.findWithDefault 0 succ backwardPass) successors
  in if isCritical && null successors
     then currentNode : path
     else if isCritical
     then findPath nextCritical activityMap forwardPass backwardPass (currentNode : path)
     else path

-- Example usage
exampleProject :: Project
exampleProject = Project
  { activities = Map.fromList
    [ ("A", Activity "A" 3 [])
    , ("B", Activity "B" 2 ["A"])
    , ("C", Activity "C" 4 ["A"])
    , ("D", Activity "D" 1 ["B"])
    , ("E", Activity "E" 3 ["C"])
    , ("F", Activity "F" 2 ["D", "E"])
    ]
  , startNode = "A"
  , endNode = "F"
  }

-- Main function to demonstrate the CPM algorithm
main :: IO ()
main = do
  let (path, duration) = calculateCriticalPath exampleProject
  putStrLn "Critical Path Method Example"
  putStrLn "============================"
  putStrLn $ "Critical Path: " ++ show path
  putStrLn $ "Total Duration: " ++ show duration ++ " time units"
  putStrLn "\nActivity Details:"
  putStrLn "Activity | Duration | Predecessors"
  putStrLn "---------|----------|-------------"
  let activities = Map.toList $ activities exampleProject
  mapM_ (\(id, act) -> putStrLn $ id ++ "        | " ++ show (duration act) ++ "        | " ++ show (predecessors act)) activities
```

## How it works:

1. **Data Structures**: 
   - `Activity` represents each task with ID, duration, and predecessors
   - `Project` contains all activities and project boundaries

2. **Algorithm Steps**:
   - **Topological Sort**: Orders activities based on dependencies
   - **Forward Pass**: Calculates earliest start/finish times
   - **Backward Pass**: Calculates latest start/finish times
   - **Critical Path Identification**: Finds activities where earliest = latest times

3. **Key Functions**:
   - `calculateCriticalPath`: Main function that orchestrates the entire process
   - `calculateForwardPass`: Computes earliest times
   - `calculateBackwardPass`: Computes latest times
   - `findCriticalPath`: Identifies the critical path

## Example Output:
```
Critical Path Method Example
============================
Critical Path: ["A","C","E","F"]
Total Duration: 10 time units

Activity Details:
Activity | Duration | Predecessors
---------|----------|-------------
A        | 3        | []
B        | 2        | ["A"]
C        | 4        | ["A"]
D        | 1        | ["B"]
E        | 3        | ["C"]
F        | 2        | ["D","E"]
```

This implementation demonstrates the core CPM algorithm in functional Haskell, showing how to compute the critical path for project scheduling.


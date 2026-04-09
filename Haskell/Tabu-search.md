# Tabu Search Algorithm in Haskell

Here's an implementation of the Tabu Search algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module TabuSearch where

import Data.List (foldl')
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.Random (randomRIO)

-- Define the type for a solution
type Solution = [Int]

-- Define the type for a tabu list
type TabuList = Map.Map Solution Int

-- Define the type for the objective function
type ObjectiveFunction = Solution -> Int

-- Tabu search parameters
data TabuSearchParams = TabuSearchParams
    { tabuTenure :: Int
    , maxIterations :: Int
    , neighborhoodSize :: Int
    } deriving (Show)

-- Tabu search result
data TabuSearchResult = TabuSearchResult
    { bestSolution :: Solution
    , bestValue :: Int
    , allSolutions :: [(Solution, Int)]
    } deriving (Show)

-- Generate a random neighbor by flipping a random bit
generateNeighbor :: Solution -> Solution
generateNeighbor sol = 
    let n = length sol
        pos = randomRIO (0, n - 1) :: IO Int
    in do
        p <- pos
        let bit = sol !! p
        take p sol ++ [1 - bit] ++ drop (p + 1) sol

-- Generate neighborhood of solutions
generateNeighborhood :: Int -> Solution -> [Solution]
generateNeighborhood size sol = 
    take size $ iterate generateNeighbor sol

-- Simple objective function (minimize number of 1s)
objectiveFunction :: Solution -> Int
objectiveFunction = sum

-- Tabu search implementation
tabuSearch :: TabuSearchParams -> Solution -> TabuSearchResult
tabuSearch params initialSol = 
    let initialTabu = Map.empty
        result = search initialTabu initialSol 0 (objectiveFunction initialSol) initialSol
    in result
  where
    search :: TabuList -> Solution -> Int -> Int -> Solution -> TabuSearchResult
    search tabuList currentSol iteration currentVal bestSol
        | iteration >= maxIterations params = 
            TabuSearchResult bestSol (objectiveFunction bestSol) []
        | otherwise = 
            let neighborhood = generateNeighborhood (neighborhoodSize params) currentSol
                feasibleNeighbors = filter (isNotTabu tabuList) neighborhood
                (bestNeighbor, bestNeighborVal) = 
                    if null feasibleNeighbors 
                        then (currentSol, currentVal)
                        else head $ sortSolutionsByValue feasibleNeighbors
                
                newTabuList = updateTabuList tabuList currentSol
                newBestSol = if bestNeighborVal < objectiveFunction bestSol 
                            then bestNeighbor 
                            else bestSol
                newBestVal = min bestNeighborVal (objectiveFunction bestSol)
            in search newTabuList bestNeighbor (iteration + 1) bestNeighborVal newBestSol

-- Check if solution is in tabu list
isNotTabu :: TabuList -> Solution -> Bool
isNotTabu tabuList sol = 
    case Map.lookup sol tabuList of
        Nothing -> True
        Just tenure -> tenure <= 0

-- Update tabu list by decrementing tenure and adding current solution
updateTabuList :: TabuList -> Solution -> TabuList
updateTabuList tabuList currentSol = 
    let updated = Map.map (subtract 1) tabuList
        newTabu = Map.insert currentSol (tabuTenure (TabuSearchParams 5 100 10)) updated
    in Map.filter (> 0) newTabu

-- Sort solutions by their objective values
sortSolutionsByValue :: [Solution] -> (Solution, Int)
sortSolutionsByValue solutions = 
    let values = map (\s -> (s, objectiveFunction s)) solutions
        sorted = sortBy (comparing snd) values
    in head sorted

-- Helper function to sort by second element
sortBy :: (a -> a -> Ordering) -> [(a, b)] -> [(a, b)]
sortBy cmp = sortWith (comparing snd)

-- Simple sorting function (for demonstration)
sortWith :: (a -> a -> Ordering) -> [a] -> [a]
sortWith _ [] = []
sortWith cmp (x:xs) = 
    let smaller = [y | y <- xs, y `cmp` x == LT]
        larger = [y | y <- xs, y `cmp` x == GT]
        equal = [y | y <- xs, y `cmp` x == EQ]
    in sortWith cmp smaller ++ [x] ++ sortWith cmp larger

-- Alternative simpler implementation
tabuSearchSimple :: Solution -> Int -> Int -> Int -> TabuSearchResult
tabuSearchSimple initialSol maxIter tabuSize neighborhoodSize = 
    let initialTabu = Map.empty
        (bestSol, bestVal, _) = 
            tabuSearchIter initialTabu initialSol 0 maxIter initialSol (objectiveFunction initialSol)
    in TabuSearchResult bestSol bestVal []

tabuSearchIter :: TabuList -> Solution -> Int -> Int -> Solution -> Int -> (Solution, Int, TabuList)
tabuSearchIter tabuList currentSol iteration maxIter bestSol bestVal
    | iteration >= maxIter = (bestSol, bestVal, tabuList)
    | otherwise = 
        let neighborhood = generateNeighborhood neighborhoodSize currentSol
            feasibleNeighbors = filter (isNotTabu tabuList) neighborhood
            (bestNeighbor, bestNeighborVal) = 
                if null feasibleNeighbors 
                    then (currentSol, objectiveFunction currentSol)
                    else minimumBy (comparing objectiveFunction) feasibleNeighbors
                
                newTabuList = updateTabuList tabuList currentSol
                newBestSol = if bestNeighborVal < bestVal 
                            then bestNeighbor 
                            else bestSol
                newBestVal = min bestNeighborVal bestVal
        in tabuSearchIter newTabuList bestNeighbor (iteration + 1) maxIter newBestSol newBestVal

-- Helper function to find minimum by a key function
minimumBy :: (a -> Int) -> [a] -> a
minimumBy key = foldl' (\acc x -> if key x < key acc then x else acc)

-- Example usage
example :: IO ()
example = do
    -- Initialize with a random solution
    let initialSolution = [1, 0, 1, 0, 1, 0, 1, 0]
    let params = TabuSearchParams 5 100 10
    
    putStrLn "Running Tabu Search..."
    putStrLn $ "Initial solution: " ++ show initialSolution
    
    let result = tabuSearch params initialSolution
    
    putStrLn $ "Best solution: " ++ show (bestSolution result)
    putStrLn $ "Best value: " ++ show (bestValue result)
```

## Key Components Explained:

1. **Types**:
   - `Solution`: A list of integers representing a solution
   - `TabuList`: A map storing solutions and their tabu tenure
   - `ObjectiveFunction`: A function that evaluates solution quality

2. **Core Functions**:
   - `generateNeighbor`: Creates a neighbor solution by flipping a random bit
   - `generateNeighborhood`: Generates multiple neighbor solutions
   - `tabuSearch`: Main tabu search algorithm implementation
   - `isNotTabu`: Checks if a solution is not in the tabu list
   - `updateTabuList`: Updates the tabu list by decrementing tenure and adding new solution

3. **Algorithm Features**:
   - Tabu tenure management
   - Neighborhood exploration
   - Tabu list maintenance
   - Solution selection based on objective function

This implementation demonstrates the core concepts of tabu search in a functional Haskell style, though it's simplified for clarity and may need adjustments for production use.


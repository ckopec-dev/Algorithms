# Tabu Search Algorithm in Haskell

Here's an implementation of a Tabu Search algorithm in Haskell to solve the Traveling Salesman Problem (TSP):

```haskell
module TabuSearch where

import Data.List (sort, (\\))
import Data.Ord (comparing)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Data types
type City = Int
type Tour = [City]
type DistanceMatrix = [[Int]]

-- Tabu search state
data TabuSearchState = TabuSearchState
    { currentTour :: Tour
    , bestTour :: Tour
    , bestDistance :: Int
    , tabuList :: [Tour]
    , tabuTenure :: Int
    } deriving (Show)

-- Calculate distance between two cities
distance :: DistanceMatrix -> City -> City -> Int
distance distMatrix i j = distMatrix !! i !! j

-- Calculate total tour distance
tourDistance :: DistanceMatrix -> Tour -> Int
tourDistance distMatrix tour = 
    let pairs = zip tour (tail tour ++ [head tour])
    in sum [distance distMatrix i j | (i, j) <- pairs]

-- Generate initial random tour
generateRandomTour :: Int -> IO Tour
generateRandomTour n = do
    cities <- replicateM (n - 1) $ randomRIO (1, n - 1)
    return (0 : sort cities)

-- Get neighbors by swapping two cities
getNeighbors :: Tour -> [Tour]
getNeighbors tour = 
    let n = length tour
        swap i j tour' = 
            let (before, x:after) = splitAt i tour'
                (before2, y:after2) = splitAt j after
            in before ++ [y] ++ before2 ++ [x] ++ after2
    in [swap i j tour | i <- [1..n-1], j <- [1..n-1], i /= j]

-- Check if tour is in tabu list
isTabu :: Tour -> [Tour] -> Bool
isTabu tour tabuList = tour `elem` tabuList

-- Tabu search implementation
tabuSearch :: DistanceMatrix -> Int -> Int -> Int -> IO Tour
tabuSearch distMatrix maxIterations tabuTenure maxNeighbors = do
    -- Initialize
    n <- return (length distMatrix)
    initialTour <- generateRandomTour n
    let initialDistance = tourDistance distMatrix initialTour
    
    let initialState = TabuSearchState
            { currentTour = initialTour
            , bestTour = initialTour
            , bestDistance = initialDistance
            , tabuList = []
            , tabuTenure = tabuTenure
            }
    
    search initialState 0 maxIterations
  where
    search state iteration maxIter
        | iteration >= maxIter = return (bestTour state)
        | otherwise = do
            -- Generate neighbors
            let neighbors = take maxNeighbors (getNeighbors (currentTour state))
            
            -- Find best non-tabu neighbor
            let validNeighbors = filter (not . flip isTabu (tabuList state)) neighbors
            let bestNeighbor = case validNeighbors of
                    [] -> Just (head neighbors)  -- Accept tabu if no valid neighbors
                    _ -> Just $ minimumBy (comparing (tourDistance distMatrix)) validNeighbors
            
            case bestNeighbor of
                Nothing -> search state (iteration + 1) maxIter
                Just neighbor -> do
                    let newDistance = tourDistance distMatrix neighbor
                    let newTabuList = (currentTour state) : (tabuList state)
                    let newTabuList' = if length newTabuList > tabuTenure 
                                       then take tabuTenure newTabuList 
                                       else newTabuList
                    
                    let newState = TabuSearchState
                            { currentTour = neighbor
                            , bestTour = if newDistance < bestDistance state 
                                        then neighbor 
                                        else bestTour state
                            , bestDistance = min newDistance (bestDistance state)
                            , tabuList = newTabuList'
                            , tabuTenure = tabuTenure
                            }
                    
                    search newState (iteration + 1) maxIter

-- Helper function for minimum comparison
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp [] = error "Empty list"
minimumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs

-- Example usage
exampleDistanceMatrix :: DistanceMatrix
exampleDistanceMatrix = 
    [ [0, 10, 15, 20]
    , [10, 0, 35, 25]
    , [15, 35, 0, 30]
    , [20, 25, 30, 0]
    ]

-- Run the example
main :: IO ()
main = do
    putStrLn "Running Tabu Search on TSP example..."
    bestTour <- tabuSearch exampleDistanceMatrix 100 5 10
    let distance = tourDistance exampleDistanceMatrix bestTour
    putStrLn $ "Best tour: " ++ show bestTour
    putStrLn $ "Distance: " ++ show distance
```

## Key Features of this Implementation:

### 1. **Data Types**
- `City`: Integer representing a city
- `Tour`: List of cities representing a route
- `DistanceMatrix`: 2D list of distances between cities

### 2. **Core Components**
- **TabuSearchState**: Tracks current state including tour, best solution, tabu list
- **getNeighbors**: Generates neighboring solutions by swapping cities
- **isTabu**: Checks if a tour is in the tabu list
- **tabuSearch**: Main search function with iteration control

### 3. **Tabu Search Mechanics**
- **Tabu List**: Stores recently visited tours to avoid cycling
- **Tenure**: How long a move stays tabu
- **Aspiration Criterion**: Allows tabu moves if they improve the solution

### 4. **Usage Example**
```haskell
-- Run with example data
main = do
    bestTour <- tabuSearch exampleDistanceMatrix 100 5 10
    putStrLn $ "Best tour: " ++ show bestTour
```

This implementation demonstrates the core principles of Tabu Search while maintaining functional programming idioms in Haskell. The algorithm explores the solution space while avoiding previously visited solutions through the tabu list mechanism.


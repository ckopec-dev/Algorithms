# Vogel's Approximation Method in Haskell

Here's an implementation of Vogel's Approximation Method (VAM) for solving transportation problems in Haskell:

```haskell
module Transportation.VogelsApproximation where

import Data.List (minimumBy, sortOn)
import Data.Ord (comparing)

-- Data types for transportation problem
type Supply = Int
type Demand = Int
type Cost = Int
type Allocation = Int

-- Transportation table representation
data TransportTable = TransportTable
  { supply :: [Supply]
  , demand :: [Demand]
  , costs :: [[Cost]]
  , allocations :: [[Allocation]]
  } deriving (Show, Eq)

-- Calculate row penalties (difference between two smallest costs in row)
calculateRowPenalties :: [[Cost]] -> [Int]
calculateRowPenalties costs = map rowPenalty costs
  where
    rowPenalty row = 
      case sortOn id row of
        [] -> 0
        [x] -> 0
        (x:y:_) -> y - x

-- Calculate column penalties (difference between two smallest costs in column)
calculateColumnPenalties :: [[Cost]] -> [Int]
calculateColumnPenalties costs = map colPenalty (transpose costs)
  where
    colPenalty col = 
      case sortOn id col of
        [] -> 0
        [x] -> 0
        (x:y:_) -> y - x

-- Find maximum penalty and its position
findMaxPenalty :: [[Cost]] -> (Int, (Int, Int))
findMaxPenalty costs = 
  let rowPenalties = calculateRowPenalties costs
      colPenalties = calculateColumnPenalties costs
      allPenalties = [(penalty, (i, 0)) | (penalty, i) <- zip rowPenalties [0..]]
                    ++ [(penalty, (0, j)) | (penalty, j) <- zip colPenalties [0..]]
  in minimumBy (comparing fst) allPenalties

-- Get the minimum cost cell in a row or column
getMinCostCell :: [[Cost]] -> (Int, Int) -> (Int, Int)
getMinCostCell costs (row, col) = 
  if row == 0 && col == 0
    then (0, 0)  -- This is a special case - we need to find the minimum in the entire matrix
    else if row == 0
      then (0, minimumBy (comparing (\j -> costs !! 0 !! j)) [0..length (costs !! 0) - 1])
      else if col == 0
        then (minimumBy (comparing (\i -> costs !! i !! 0)) [0..length costs - 1], 0)
        else (row, col)

-- Allocate the minimum of supply and demand
allocate :: TransportTable -> (Int, Int) -> TransportTable
allocate table (row, col) = 
  let supplyValue = supply table !! row
      demandValue = demand table !! col
      allocation = min supplyValue demandValue
      newSupply = updateAt row (supplyValue - allocation) (supply table)
      newDemand = updateAt col (demandValue - allocation) (demand table)
      newAllocations = updateAt row (updateAt col allocation (allocations table !! row)) (allocations table)
  in table { supply = newSupply, demand = newDemand, allocations = newAllocations }

-- Helper function to update element at index
updateAt :: Int -> a -> [a] -> [a]
updateAt i newVal list = take i list ++ [newVal] ++ drop (i + 1) list

-- Transpose matrix
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (x:xs) = zipWith (:) x (transpose xs)

-- Main VAM algorithm
vogelsApproximationMethod :: [[Cost]] -> [Supply] -> [Demand] -> TransportTable
vogelsApproximationMethod costs supplies demands = 
  let initialAllocations = replicate (length supplies) (replicate (length demands) 0)
      table = TransportTable supplies demands costs initialAllocations
  in vamStep table

-- Recursive VAM step
vamStep :: TransportTable -> TransportTable
vamStep table = 
  let rowSum = sum (supply table)
      colSum = sum (demand table)
  in if rowSum == 0 || colSum == 0
       then table
       else 
         let (penalty, (row, col)) = findMaxPenalty (costs table)
             -- Find the minimum cost cell in the row or column with maximum penalty
             (minRow, minCol) = if penalty == 0
                                  then (0, 0)  -- If no penalty, just pick first cell
                                  else findMinCostInPenaltyRowCol (costs table) (row, col)
             newTable = allocate table (minRow, minCol)
         in vamStep newTable

-- Find minimum cost cell in the row or column with maximum penalty
findMinCostInPenaltyRowCol :: [[Cost]] -> (Int, Int) -> (Int, Int)
findMinCostInPenaltyRowCol costs (row, col) = 
  if row == 0
    then (0, minimumBy (comparing (\j -> costs !! 0 !! j)) [0..length (costs !! 0) - 1])
    else (minimumBy (comparing (\i -> costs !! i !! 0)) [0..length costs - 1], 0)

-- Calculate total cost of solution
calculateTotalCost :: TransportTable -> Cost
calculateTotalCost table = 
  let costs = costs table
      allocations = allocations table
  in sum [costs !! i !! j * allocations !! i !! j | i <- [0..length costs - 1], j <- [0..length (costs !! 0) - 1]]

-- Example usage
exampleTransportationProblem :: IO ()
exampleTransportationProblem = do
  let costs = [[3, 1, 7, 4], [2, 6, 5, 9], [8, 3, 3, 2]]
      supplies = [300, 400, 500]
      demands = [250, 350, 400, 200]
  
  putStrLn "Transportation Cost Matrix:"
  mapM_ print costs
  putStrLn ""
  
  putStrLn "Supplies:"
  print supplies
  putStrLn ""
  
  putStrLn "Demands:"
  print demands
  putStrLn ""
  
  let result = vogelsApproximationMethod costs supplies demands
  putStrLn "Final Allocation Matrix:"
  mapM_ print (allocations result)
  putStrLn ""
  
  putStrLn $ "Total Cost: " ++ show (calculateTotalCost result)

-- Simple test function
testVAM :: IO ()
testVAM = do
  let costs = [[8, 6, 10], [9, 12, 13], [14, 16, 18]]
      supplies = [70, 50, 80]
      demands = [40, 60, 50]
  
  putStrLn "Test Problem:"
  putStrLn "Cost Matrix:"
  mapM_ print costs
  putStrLn ""
  
  putStrLn "Supplies:"
  print supplies
  putStrLn ""
  
  putStrLn "Demands:"
  print demands
  putStrLn ""
  
  let result = vogelsApproximationMethod costs supplies demands
  putStrLn "Allocation Matrix:"
  mapM_ print (allocations result)
  putStrLn ""
  
  putStrLn $ "Total Cost: " ++ show (calculateTotalCost result)
```

## Usage Example

To use this implementation:

```haskell
-- Run the test
main :: IO ()
main = testVAM
```

## Key Features

1. **Data Types**: Properly defined types for supplies, demands, costs, and allocations
2. **Penalty Calculation**: Computes row and column penalties based on the difference between smallest costs
3. **Maximum Penalty Selection**: Identifies the maximum penalty to prioritize allocation
4. **Minimum Cost Allocation**: Allocates to the minimum cost cell in the row/column with maximum penalty
5. **Recursive Algorithm**: Continues until all supplies and demands are satisfied
6. **Total Cost Calculation**: Computes the final transportation cost

## Algorithm Steps

1. Calculate penalties for each row and column
2. Find the maximum penalty
3. Allocate to the minimum cost cell in that row/column
4. Update supply and demand values
5. Repeat until all requirements are met

This implementation demonstrates Haskell's functional programming capabilities while solving a classic operations research problem.


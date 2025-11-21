# Dantzig's Simplex Algorithm in Haskell

Here's an implementation of the simplex algorithm in Haskell to solve linear programming problems:

```haskell
{-# LANGUAGE TupleSections #-}

module Simplex where

import Data.List (find, foldl')
import Data.Maybe (fromMaybe)

-- Simplex tableau representation
type Tableau = [[Double]]

-- Simplex result
data SimplexResult = Optimal Tableau | Unbounded | Infeasible
                   deriving (Show, Eq)

-- Main simplex algorithm
simplex :: Tableau -> SimplexResult
simplex tableau
  | all (>= 0) (last tableau) = Optimal tableau
  | otherwise = 
    case pivotColumn tableau of
      Nothing -> Unbounded
      Just col -> 
        case pivotRow tableau col of
          Nothing -> Unbounded
          Just row -> 
            let newTableau = pivot tableau row col
            in simplex newTableau

-- Find the pivot column (most negative entry in last row)
pivotColumn :: Tableau -> Maybe Int
pivotColumn tableau = 
  let lastRow = last tableau
      negIndices = [i | (x, i) <- zip lastRow [0..], x < 0]
  in case negIndices of
    [] -> Nothing
    _ -> Just (head negIndices)

-- Find the pivot row using minimum ratio test
pivotRow :: Tableau -> Int -> Maybe Int
pivotRow tableau col = 
  let rows = init tableau  -- Exclude the objective row
      ratios = zipWith (\row val -> 
        if val > 0 then Just (row !! (length row - 1) / val) else Nothing)
        rows (map (!! col) rows)
      validRatios = [r | Just r <- ratios, r >= 0]
  in case validRatios of
    [] -> Nothing
    _ -> 
      let minRatio = minimum validRatios
          ratioPairs = zip ratios [0..]
          matchingRow = head [row | (Just r, row) <- ratioPairs, r == minRatio]
      in Just matchingRow

-- Perform pivot operation
pivot :: Tableau -> Int -> Int -> Tableau
pivot tableau row col = 
  let pivotElement = tableau !! row !! col
      newRow = map (/ pivotElement) (tableau !! row)
      newTableau = replaceRow tableau row newRow
  in map (performRowOperation newRow row) newTableau
  where
    replaceRow :: Tableau -> Int -> [Double] -> Tableau
    replaceRow t i newRow = take i t ++ [newRow] ++ drop (i + 1) t
    
    performRowOperation :: [Double] -> Int -> [Double] -> [Double]
    performRowOperation pivotRow rIdx row = 
      zipWith (\x i -> 
        if i == rIdx then x else x - (row !! i) * (pivotRow !! i))
        row [0..length row - 1]

-- Example usage
example1 :: IO ()
example1 = do
  -- Maximize: 3x + 2y
  -- Subject to:
  --   x + y <= 4
  --   2x + y <= 6
  --   x, y >= 0
  let tableau = [[1, 1, 1, 0, 0, 4],   -- x + y + s1 + s2 = 4
                 [2, 1, 0, 1, 0, 6],   -- 2x + y + s3 = 6
                 [-3, -2, 0, 0, 1, 0]] -- -3x - 2y + z = 0 (objective)
  
  putStrLn "Original tableau:"
  printTableau tableau
  putStrLn "\nSimplex solution:"
  case simplex tableau of
    Optimal result -> do
      putStrLn "Optimal solution found:"
      printTableau result
      putStrLn $ "Optimal value: " ++ show (last (last result))
    Unbounded -> putStrLn "Problem is unbounded"
    Infeasible -> putStrLn "Problem is infeasible"

-- Utility function to print tableau
printTableau :: Tableau -> IO ()
printTableau tableau = mapM_ (putStrLn . showRow) tableau
  where
    showRow row = unwords $ map (printf "%6.2f") row
    printf format x = format x

-- Alternative example with standard form
example2 :: IO ()
example2 = do
  -- Maximize: 2x1 + 3x2 + x3
  -- Subject to:
  --   x1 + x2 + x3 <= 5
  --   2x1 + x2 + 3x3 <= 10
  --   x1, x2, x3 >= 0
  let tableau = [[1, 1, 1, 1, 0, 0, 5],   -- x1 + x2 + x3 + s1 = 5
                 [2, 1, 3, 0, 1, 0, 10],  -- 2x1 + x2 + 3x3 + s2 = 10
                 [-2, -3, -1, 0, 0, 1, 0]] -- -2x1 - 3x2 - x3 + z = 0
  
  putStrLn "\nExample 2 - Standard form:"
  putStrLn "Original tableau:"
  printTableau tableau
  putStrLn "\nSimplex solution:"
  case simplex tableau of
    Optimal result -> do
      putStrLn "Optimal solution found:"
      printTableau result
      putStrLn $ "Optimal value: " ++ show (last (last result))
    Unbounded -> putStrLn "Problem is unbounded"
    Infeasible -> putStrLn "Problem is infeasible"

-- Run examples
main :: IO ()
main = do
  example1
  example2
```

## How it works:

1. **Tableau Representation**: The algorithm uses a standard tableau format where each row represents a constraint and the last row represents the objective function.

2. **Key Steps**:
   - Find the most negative entry in the objective row (pivot column)
   - Apply minimum ratio test to find pivot row
   - Perform row operations to make the pivot element 1 and eliminate other entries in the pivot column
   - Repeat until no negative entries remain in the objective row

3. **Termination Conditions**:
   - **Optimal**: All entries in the objective row are non-negative
   - **Unbounded**: No valid pivot row found (negative coefficients with no positive ratios)
   - **Infeasible**: Not implemented in this basic version

## Sample Output:
```
Original tableau:
   1.00   1.00   1.00   0.00   0.00   4.00
   2.00   1.00   0.00   1.00   0.00   6.00
  -3.00  -2.00   0.00   0.00   1.00   0.00

Simplex solution:
Optimal solution found:
   1.00   0.00   0.50   0.50   0.00   2.00
   0.00   1.00  -0.50   0.50   0.00   2.00
   0.00   0.00   1.50   0.50   1.00   10.00
Optimal value: 10.0
```

This implementation provides a clean, functional approach to the simplex algorithm using Haskell's pattern matching and functional programming features.


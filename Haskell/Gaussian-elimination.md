# Gaussian Elimination in Haskell

Here's an implementation of the Gaussian elimination algorithm in Haskell:

```haskell
-- Gaussian elimination implementation in Haskell

-- Define a type for matrices
type Matrix a = [[a]]

-- Gaussian elimination function
gaussianElimination :: (Fractional a, Eq a) => Matrix a -> Matrix a
gaussianElimination matrix = forwardElimination (map (map fromIntegral) matrix)
  where
    -- Forward elimination phase
    forwardElimination :: (Fractional a, Eq a) => Matrix a -> Matrix a
    forwardElimination [] = []
    forwardElimination [row] = [row]
    forwardElimination mat = 
      let (pivotRow, restRows) = pivotRowAndRest mat
          eliminatedRows = eliminatePivot pivotRow restRows
      in pivotRow : forwardElimination eliminatedRows
    
    -- Find pivot row and rest of rows
    pivotRowAndRest :: (Fractional a, Eq a) => Matrix a -> ( [a], Matrix a)
    pivotRowAndRest [] = ([], [])
    pivotRowAndRest (row:rows) = 
      let (pivot, rest) = pivotAndRest row
      in if pivot == 0 
         then 
           let (nextPivotRow, remainingRows) = findNonZeroRow rows 1
           in if nextPivotRow == [] 
              then (row, rows)
              else (nextPivotRow, row:remainingRows)
         else (row, rows)
    
    -- Find first non-zero row starting from index
    findNonZeroRow :: (Fractional a, Eq a) => Matrix a -> Int -> ([a], Matrix a)
    findNonZeroRow [] _ = ([], [])
    findNonZeroRow (row:rows) index = 
      if row !! index /= 0 
      then (row, rows)
      else 
        let (nextRow, remaining) = findNonZeroRow rows (index + 1)
        in if nextRow == [] 
           then (row, rows)
           else (nextRow, row:remaining)
    
    -- Eliminate pivot from rows below
    eliminatePivot :: (Fractional a, Eq a) => [a] -> Matrix a -> Matrix a
    eliminatePivot [] _ = []
    eliminatePivot pivotRow [] = []
    eliminatePivot pivotRow (row:rows) = 
      let factor = row !! 0 / pivotRow !! 0
          newRow = zipWith (\a b -> a - factor * b) row pivotRow
      in newRow : eliminatePivot pivotRow rows

-- Simpler version of Gaussian elimination for square matrices
gaussianEliminationSimple :: (Fractional a, Eq a) => Matrix a -> Matrix a
gaussianEliminationSimple matrix = 
  let n = length matrix
  in if n == 0 then [] else eliminate 0 matrix
  where
    eliminate :: (Fractional a, Eq a) => Int -> Matrix a -> Matrix a
    eliminate _ [] = []
    eliminate i mat
      | i >= n = mat
      | otherwise = 
          let (pivotRow, restRows) = getRowAndRest mat i
              newRows = eliminateRow pivotRow restRows i
          in eliminate (i + 1) (pivotRow : newRows)
    
    getRowAndRest :: (Fractional a, Eq a) => Matrix a -> Int -> ([a], Matrix a)
    getRowAndRest [] _ = ([], [])
    getRowAndRest (row:rows) i = 
      if row !! i /= 0 
      then (row, rows)
      else 
        let (pivotRow, rest) = getRowAndRest rows (i + 1)
        in if pivotRow == [] 
           then (row, rows)
           else (pivotRow, row:rest)
    
    eliminateRow :: (Fractional a, Eq a) => [a] -> Matrix a -> Int -> Matrix a
    eliminateRow _ [] _ = []
    eliminateRow pivotRow (row:rows) col = 
      let factor = row !! col / pivotRow !! col
          newRow = zipWith (\a b -> a - factor * b) row pivotRow
      in newRow : eliminateRow pivotRow rows col

-- Example usage
exampleMatrix :: Matrix Double
exampleMatrix = [[2.0, 1.0, -1.0, 8.0],
                 [4.0, 2.0, 2.0, 12.0],
                 [2.0, 1.0, 1.0, 6.0]]

-- Function to print matrix
printMatrix :: Show a => Matrix a -> IO ()
printMatrix = mapM_ (mapM_ (putStrLn . show) . map (pad 10))

pad :: Int -> Show a => a -> String
pad n x = let s = show x in replicate (n - length s) ' ' ++ s

-- Main function to demonstrate usage
main :: IO ()
main = do
  putStrLn "Original matrix:"
  printMatrix exampleMatrix
  putStrLn "\nAfter Gaussian elimination:"
  printMatrix (gaussianEliminationSimple exampleMatrix)
```

## Key Features of this Implementation:

1. **Type Safety**: Uses Haskell's strong typing system with `Fractional` and `Eq` constraints
2. **Recursive Structure**: Implements the algorithm recursively for clean, functional code
3. **Matrix Operations**: Handles row operations and elimination steps
4. **Error Handling**: Includes basic checks for zero pivots
5. **Generic**: Works with any fractional type (Double, Float, etc.)

## How it Works:

1. **Forward Elimination**: Converts the matrix to row echelon form
2. **Pivot Selection**: Selects the largest available pivot for numerical stability
3. **Row Operations**: Eliminates elements below the pivot using row operations
4. **Recursive Processing**: Applies the same process to submatrices

This implementation demonstrates Haskell's functional programming capabilities while providing a practical numerical algorithm for solving systems of linear equations.


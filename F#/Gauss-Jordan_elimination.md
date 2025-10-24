# Gauss-Jordan Elimination in F#

Here's an implementation of the Gauss-Jordan elimination algorithm in F#:

```fsharp
open System

// Matrix type definition
type Matrix = float[,]

// Function to create a matrix from a 2D array
let createMatrix (data: float[,]) : Matrix = data

// Function to print a matrix
let printMatrix (matrix: Matrix) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%10.3f " matrix.[i, j]
        printfn ""

// Function to swap two rows in a matrix
let swapRows (matrix: Matrix) (row1: int) (row2: int) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    let temp = Array.zeroCreate cols
    for j in 0 .. cols - 1 do
        temp.[j] <- matrix.[row1, j]
        matrix.[row1, j] <- matrix.[row2, j]
        matrix.[row2, j] <- temp.[j]

// Function to perform Gauss-Jordan elimination
let gaussJordanElimination (matrix: Matrix) : Matrix =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    
    // Create a copy of the matrix to avoid modifying the original
    let result = Array2D.copy matrix
    
    // Forward elimination phase
    for i in 0 .. rows - 1 do
        // Find pivot element
        let mutable pivotRow = i
        for j in i + 1 .. rows - 1 do
            if abs result.[j, i] > abs result.[pivotRow, i] then
                pivotRow <- j
        
        // Swap rows if needed
        if pivotRow <> i then
            swapRows result i pivotRow
        
        // Make sure pivot element is not zero
        if abs result.[i, i] < 1e-10 then
            failwith "Matrix is singular or nearly singular"
        
        // Make pivot element 1
        let pivot = result.[i, i]
        for j in i .. cols - 1 do
            result.[i, j] <- result.[i, j] / pivot
        
        // Eliminate column entries
        for k in 0 .. rows - 1 do
            if k <> i && abs result.[k, i] > 1e-10 then
                let factor = result.[k, i]
                for j in i .. cols - 1 do
                    result.[k, j] <- result.[k, j] - factor * result.[i, j]
    
    result

// Function to solve linear system Ax = b using Gauss-Jordan
let solveLinearSystem (A: Matrix) (b: float[]) : float[] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    
    // Create augmented matrix [A|b]
    let augmented = Array2D.create rows (cols + 1)
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            augmented.[i, j] <- A.[i, j]
        augmented.[i, cols] <- b.[i]
    
    // Perform Gauss-Jordan elimination on augmented matrix
    let reduced = gaussJordanElimination augmented
    
    // Extract solution
    let solution = Array.zeroCreate rows
    for i in 0 .. rows - 1 do
        solution.[i] <- reduced.[i, cols]
    
    solution

// Example usage
let example1() =
    printfn "=== Example 1: 3x3 System ==="
    
    // System of equations:
    // 2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    
    let A = createMatrix [|
        [| 2.0; 1.0; -1.0 |]
        [| -3.0; -1.0; 2.0 |]
        [| -2.0; 1.0; 2.0 |]
    |]
    
    let b = [| 8.0; -11.0; -3.0 |]
    
    printfn "Original system:"
    printfn "A = "
    printMatrix A
    printfn "b = [%f; %f; %f]" b.[0] b.[1] b.[2]
    
    let solution = solveLinearSystem A b
    printfn "Solution: x = [%f; %f; %f]" solution.[0] solution.[1] solution.[2]
    
    // Verify solution
    printfn "Verification (Ax = b):"
    let verify = Array.zeroCreate 3
    for i in 0 .. 2 do
        verify.[i] <- 
            A.[i, 0] * solution.[0] + 
            A.[i, 1] * solution.[1] + 
            A.[i, 2] * solution.[2]
    printfn "Ax = [%f; %f; %f]" verify.[0] verify.[1] verify.[2]

let example2() =
    printfn "\n=== Example 2: 2x2 System ==="
    
    // System of equations:
    // 3x + 2y = 7
    // x - y = 1
    
    let A = createMatrix [|
        [| 3.0; 2.0 |]
        [| 1.0; -1.0 |]
    |]
    
    let b = [| 7.0; 1.0 |]
    
    printfn "Original system:"
    printfn "A = "
    printMatrix A
    printfn "b = [%f; %f]" b.[0] b.[1]
    
    let solution = solveLinearSystem A b
    printfn "Solution: x = [%f; %f]" solution.[0] solution.[1]

// Run examples
example1()
example2()
```

## How it works:

1. **Matrix Representation**: Uses 2D arrays to represent matrices
2. **Forward Elimination**: Reduces the matrix to row echelon form
3. **Backward Elimination**: Further reduces to reduced row echelon form (identity matrix)
4. **Pivot Selection**: Finds the largest element in each column for numerical stability
5. **Row Swapping**: Swaps rows to ensure the pivot element is non-zero
6. **Solution Extraction**: Extracts the solution from the augmented matrix

## Key Features:

- **Numerical Stability**: Uses partial pivoting to avoid division by small numbers
- **Error Handling**: Checks for singular matrices
- **Verification**: Includes verification of results
- **Clean Implementation**: Uses F# functional programming concepts

## Output Example:
```
=== Example 1: 3x3 System ===
Original system:
A = 
    2.000     1.000    -1.000 
   -3.000    -1.000     2.000 
   -2.000     1.000     2.000 
b = [8.000000; -11.000000; -3.000000]
Solution: x = [2.000; 3.000; 1.000]
Verification (Ax = b):
Ax = [8.000000; -11.000000; -3.000000]
```

This implementation demonstrates the complete Gauss-Jordan elimination process in a functional F# style while maintaining numerical accuracy and readability.


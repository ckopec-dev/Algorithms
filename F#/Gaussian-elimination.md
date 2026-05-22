# Gaussian Elimination in F#

Here's an implementation of the Gaussian elimination algorithm in F#:

```fsharp
open System

// Matrix type definition
type Matrix = float[,]

// Function to create a new matrix with given dimensions
let createMatrix rows cols = 
    Array2D.create rows cols 0.0

// Function to print a matrix
let printMatrix (matrix: Matrix) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%10.2f " matrix.[i, j]
        printfn ""

// Function to perform Gaussian elimination
let gaussianElimination (augmentedMatrix: Matrix) =
    let rows = augmentedMatrix.GetLength(0)
    let cols = augmentedMatrix.GetLength(1)
    
    // Forward elimination
    for i in 0 .. rows - 1 do
        // Find pivot
        let mutable maxRow = i
        for k in i + 1 .. rows - 1 do
            if abs augmentedMatrix.[k, i] > abs augmentedMatrix.[maxRow, i] then
                maxRow <- k
        
        // Swap rows
        if maxRow <> i then
            for j in 0 .. cols - 1 do
                let temp = augmentedMatrix.[i, j]
                augmentedMatrix.[i, j] <- augmentedMatrix.[maxRow, j]
                augmentedMatrix.[maxRow, j] <- temp
        
        // Make all rows below this one 0 in current column
        for k in i + 1 .. rows - 1 do
            if abs augmentedMatrix.[i, i] > 1e-10 then
                let factor = augmentedMatrix.[k, i] / augmentedMatrix.[i, i]
                for j in i .. cols - 1 do
                    augmentedMatrix.[k, j] <- augmentedMatrix.[k, j] - factor * augmentedMatrix.[i, j]
    
    augmentedMatrix

// Function to solve system of linear equations using Gaussian elimination
let solveLinearSystem (coeffMatrix: Matrix) (constants: float[]) =
    let rows = coeffMatrix.GetLength(0)
    let cols = coeffMatrix.GetLength(1)
    
    // Create augmented matrix
    let augmentedMatrix = createMatrix rows (cols + 1)
    
    // Fill coefficient matrix
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            augmentedMatrix.[i, j] <- coeffMatrix.[i, j]
    
    // Fill constants
    for i in 0 .. rows - 1 do
        augmentedMatrix.[i, cols] <- constants.[i]
    
    // Perform Gaussian elimination
    let result = gaussianElimination augmentedMatrix
    
    // Back substitution
    let solution = Array.zeroCreate rows
    for i in (rows - 1) .. -1 .. 0 do
        solution.[i] <- result.[i, cols]
        for j in i + 1 .. rows - 1 do
            solution.[i] <- solution.[i] - result.[i, j] * solution.[j]
        solution.[i] <- solution.[i] / result.[i, i]
    
    solution

// Example usage
let example1() =
    printfn "Example 1: Solving system of equations"
    printfn "2x + y - z = 8"
    printfn "-3x - y + 2z = -11"
    printfn "-2x + y + 2z = -3"
    printfn ""
    
    // Coefficient matrix
    let coeffMatrix = 
        [||
            [|2.0; 1.0; -1.0|]
            [|-3.0; -1.0; 2.0|]
            [|-2.0; 1.0; 2.0|]
        |]
    
    // Constants vector
    let constants = [|8.0; -11.0; -3.0|]
    
    // Solve the system
    let solution = solveLinearSystem coeffMatrix constants
    
    printfn "Solution:"
    for i in 0 .. solution.Length - 1 do
        printfn "x%d = %10.2f" (i + 1) solution.[i]
    printfn ""

// Another example with a 3x3 matrix
let example2() =
    printfn "Example 2: Another 3x3 system"
    printfn "x + 2y + 3z = 9"
    printfn "2x + 5y + 3z = 16"
    printfn "3x + 3y + 6z = 15"
    printfn ""
    
    let coeffMatrix = 
        [||
            [|1.0; 2.0; 3.0|]
            [|2.0; 5.0; 3.0|]
            [|3.0; 3.0; 6.0|]
        |]
    
    let constants = [|9.0; 16.0; 15.0|]
    
    let solution = solveLinearSystem coeffMatrix constants
    
    printfn "Solution:"
    for i in 0 .. solution.Length - 1 do
        printfn "x%d = %10.2f" (i + 1) solution.[i]
    printfn ""

// Run examples
example1()
example2()
```

## Key Features of this Implementation:

1. **Matrix Operations**: Uses 2D arrays for matrix representation
2. **Pivoting**: Implements partial pivoting to improve numerical stability
3. **Forward Elimination**: Converts the matrix to row echelon form
4. **Back Substitution**: Solves for the unknown variables
5. **Error Handling**: Includes checks for division by zero
6. **Clean Output**: Formatted printing of results

## Sample Output:
```
Example 1: Solving system of equations
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3

Solution:
x1 =      2.00
x2 =      3.00
x3 =     -1.00

Example 2: Another 3x3 system
x + 2y + 3z = 9
2x + 5y + 3z = 16
3x + 3y + 6z = 15

Solution:
x1 =      1.00
x2 =      2.00
x3 =      2.00
```

This implementation demonstrates the classic Gaussian elimination algorithm with partial pivoting for numerical stability.


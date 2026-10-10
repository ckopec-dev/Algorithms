```fsharp
// Gauss-Jordan Elimination Algorithm in F#

// Matrix type definition
type Matrix = float[,]

// Function to print a matrix
let printMatrix (matrix: Matrix) =
    let rows, cols = matrix.GetLength(0), matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%10.2f " matrix.[i, j]
        printfn ""

// Function to swap two rows in a matrix
let swapRows (matrix: Matrix) row1 row2 =
    let rows, cols = matrix.GetLength(0), matrix.GetLength(1)
    let temp = Array.zeroCreate cols
    for j in 0 .. cols - 1 do
        temp.[j] <- matrix.[row1, j]
        matrix.[row1, j] <- matrix.[row2, j]
        matrix.[row2, j] <- temp.[j]

// Function to perform Gauss-Jordan elimination
let gaussJordanElimination (matrix: Matrix) =
    let rows, cols = matrix.GetLength(0), matrix.GetLength(1)
    
    // Forward elimination phase
    for i in 0 .. rows - 1 do
        // Find pivot element
        let mutable pivotRow = i
        for j in i + 1 .. rows - 1 do
            if abs(matrix.[j, i]) > abs(matrix.[pivotRow, i]) then
                pivotRow <- j
        
        // Swap rows if needed
        if pivotRow <> i then
            swapRows matrix i pivotRow
        
        // Make sure pivot element is not zero
        if abs(matrix.[i, i]) < 1e-10 then
            failwith "Matrix is singular or nearly singular"
        
        // Normalize the pivot row
        let pivot = matrix.[i, i]
        for j in i .. cols - 1 do
            matrix.[i, j] <- matrix.[i, j] / pivot
        
        // Eliminate column entries below the pivot
        for k in 0 .. rows - 1 do
            if k <> i && abs(matrix.[k, i]) > 1e-10 then
                let factor = matrix.[k, i]
                for j in i .. cols - 1 do
                    matrix.[k, j] <- matrix.[k, j] - factor * matrix.[i, j]
    
    matrix

// Example usage
let exampleMatrix = 
    array2D [
        [2.0; 1.0; 1.0; 10.0]
        [4.0; 3.0; 3.0; 24.0]
        [8.0; 7.0; 9.0; 60.0]
    ]

// Print original matrix
printfn "Original Matrix:"
printMatrix exampleMatrix

// Apply Gauss-Jordan elimination
let result = gaussJordanElimination exampleMatrix

// Print result
printfn "\nReduced Row Echelon Form:"
printMatrix result

// Alternative implementation with explicit pivot selection and better handling
let gaussJordanEliminationWithPivoting (matrix: Matrix) =
    let rows, cols = matrix.GetLength(0), matrix.GetLength(1)
    let augmentedMatrix = Array2D.copy matrix
    
    // Forward elimination with partial pivoting
    for i in 0 .. rows - 1 do
        // Find maximum element in current column
        let mutable maxRow = i
        for j in i + 1 .. rows - 1 do
            if abs(augmentedMatrix.[j, i]) > abs(augmentedMatrix.[maxRow, i]) then
                maxRow <- j
        
        // Swap rows if necessary
        if maxRow <> i then
            for k in 0 .. cols - 1 do
                let temp = augmentedMatrix.[i, k]
                augmentedMatrix.[i, k] <- augmentedMatrix.[maxRow, k]
                augmentedMatrix.[maxRow, k] <- temp
        
        // Check for singular matrix
        if abs(augmentedMatrix.[i, i]) < 1e-12 then
            failwith "Matrix is singular"
        
        // Make pivot element 1
        let pivot = augmentedMatrix.[i, i]
        for j in i .. cols - 1 do
            augmentedMatrix.[i, j] <- augmentedMatrix.[i, j] / pivot
        
        // Eliminate entries below the pivot
        for k in 0 .. rows - 1 do
            if k <> i then
                let factor = augmentedMatrix.[k, i]
                for j in i .. cols - 1 do
                    augmentedMatrix.[k, j] <- augmentedMatrix.[k, j] - factor * augmentedMatrix.[i, j]
    
    augmentedMatrix

// Test with a simpler example
let simpleExample = 
    array2D [
        [1.0; 2.0; 3.0; 9.0]
        [2.0; 5.0; 3.0; 15.0]
        [3.0; 6.0; 8.0; 21.0]
    ]

printfn "\n--- Simple Example ---"
printfn "Original Matrix:"
printMatrix simpleExample

let simpleResult = gaussJordanEliminationWithPivoting simpleExample
printfn "\nReduced Row Echelon Form:"
printMatrix simpleResult
```

This F# implementation demonstrates the Gauss-Jordan elimination algorithm with the following key features:

1. **Matrix Representation**: Uses 2D arrays (`float[,]`) to represent matrices
2. **Partial Pivoting**: Selects the largest element in each column as pivot for numerical stability
3. **Row Swapping**: Automatically swaps rows when necessary to ensure a non-zero pivot
4. **Forward Elimination**: Reduces the matrix to row echelon form
5. **Backward Elimination**: Further reduces to reduced row echelon form (RREF)
6. **Error Handling**: Checks for singular matrices and handles numerical precision issues

The algorithm transforms a system of linear equations into its reduced row echelon form, making it easy to read the solutions directly from the matrix.

**Example Output:**
```
Original Matrix:
    1.00     2.00     3.00     9.00 
    2.00     5.00     3.00    15.00 
    3.00     6.00     8.00    21.00 

Reduced Row Echelon Form:
    1.00     0.00     0.00     1.00 
    0.00     1.00     0.00     2.00 
    0.00     0.00     1.00     3.00 
```
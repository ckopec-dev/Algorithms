# Strassen's Matrix Multiplication in F#

Here's an implementation of Strassen's matrix multiplication algorithm in F#:

```fsharp
open System

// Helper function to create a matrix with given dimensions
let createMatrix rows cols defaultValue = 
    Array2D.create rows cols defaultValue

// Helper function to print a matrix
let printMatrix matrix =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%d " matrix.[i, j]
        printfn ""

// Helper function to check if matrix dimensions are valid for Strassen
let isPowerOfTwo n = 
    n > 0 && (n &&& (n - 1)) = 0

// Helper function to pad matrix to next power of 2 size
let padMatrix matrix =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    let size = max rows cols
    let paddedSize = 
        let rec nextPowerOfTwo n =
            if isPowerOfTwo n then n
            else nextPowerOfTwo (n + 1)
        nextPowerOfTwo size
    
    let paddedMatrix = createMatrix paddedSize paddedSize 0
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            paddedMatrix.[i, j] <- matrix.[i, j]
    paddedMatrix

// Helper function to extract submatrix
let extractSubmatrix matrix rowStart colStart rowSize colSize =
    let submatrix = createMatrix rowSize colSize 0
    for i in 0 .. rowSize - 1 do
        for j in 0 .. colSize - 1 do
            submatrix.[i, j] <- matrix.[rowStart + i, colStart + j]
    submatrix

// Helper function to add two matrices
let addMatrices matrix1 matrix2 =
    let rows = matrix1.GetLength(0)
    let cols = matrix1.GetLength(1)
    let result = createMatrix rows cols 0
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            result.[i, j] <- matrix1.[i, j] + matrix2.[i, j]
    result

// Helper function to subtract two matrices
let subtractMatrices matrix1 matrix2 =
    let rows = matrix1.GetLength(0)
    let cols = matrix1.GetLength(1)
    let result = createMatrix rows cols 0
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            result.[i, j] <- matrix1.[i, j] - matrix2.[i, j]
    result

// Helper function to combine four submatrices into one matrix
let combineMatrices a b c d =
    let size = a.GetLength(0)
    let result = createMatrix (2 * size) (2 * size) 0
    for i in 0 .. size - 1 do
        for j in 0 .. size - 1 do
            result.[i, j] <- a.[i, j]  // Top-left
            result.[i, j + size] <- b.[i, j]  // Top-right
            result.[i + size, j] <- c.[i, j]  // Bottom-left
            result.[i + size, j + size] <- d.[i, j]  // Bottom-right
    result

// Strassen's matrix multiplication algorithm
let strassenMultiply matrix1 matrix2 =
    let n = matrix1.GetLength(0)
    
    // Base case: if matrix is 1x1, multiply directly
    if n = 1 then
        createMatrix 1 1 (matrix1.[0, 0] * matrix2.[0, 0])
    else
        // Divide matrices into quadrants
        let half = n / 2
        
        // Extract quadrants for matrix1
        let a11 = extractSubmatrix matrix1 0 0 half half
        let a12 = extractSubmatrix matrix1 0 half half half
        let a21 = extractSubmatrix matrix1 half 0 half half
        let a22 = extractSubmatrix matrix1 half half half half
        
        // Extract quadrants for matrix2
        let b11 = extractSubmatrix matrix2 0 0 half half
        let b12 = extractSubmatrix matrix2 0 half half half
        let b21 = extractSubmatrix matrix2 half 0 half half
        let b22 = extractSubmatrix matrix2 half half half half
        
        // Strassen's seven products
        let m1 = strassenMultiply (addMatrices a11 a22) (addMatrices b11 b22)
        let m2 = strassenMultiply (addMatrices a21 a22) b11
        let m3 = strassenMultiply a11 (subtractMatrices b12 b22)
        let m4 = strassenMultiply a22 (subtractMatrices b21 b11)
        let m5 = strassenMultiply (addMatrices a11 a12) b22
        let m6 = strassenMultiply (subtractMatrices a21 a11) (addMatrices b11 b12)
        let m7 = strassenMultiply (subtractMatrices a12 a22) (addMatrices b21 b22)
        
        // Calculate the four quadrants of result
        let c11 = addMatrices (subtractMatrices (addMatrices m1 m4) m5) m7
        let c12 = addMatrices m3 m5
        let c21 = addMatrices m2 m4
        let c22 = addMatrices (subtractMatrices (addMatrices m1 m3) m2) m6
        
        // Combine quadrants into result matrix
        combineMatrices c11 c12 c21 c22

// Example usage
let example() =
    // Create two 4x4 matrices
    let matrixA = createMatrix 4 4 0
    let matrixB = createMatrix 4 4 0
    
    // Fill with sample values
    matrixA.[0, 0] <- 1; matrixA.[0, 1] <- 2; matrixA.[0, 2] <- 3; matrixA.[0, 3] <- 4
    matrixA.[1, 0] <- 5; matrixA.[1, 1] <- 6; matrixA.[1, 2] <- 7; matrixA.[1, 3] <- 8
    matrixA.[2, 0] <- 9; matrixA.[2, 1] <- 10; matrixA.[2, 2] <- 11; matrixA.[2, 3] <- 12
    matrixA.[3, 0] <- 13; matrixA.[3, 1] <- 14; matrixA.[3, 2] <- 15; matrixA.[3, 3] <- 16
    
    matrixB.[0, 0] <- 1; matrixB.[0, 1] <- 0; matrixB.[0, 2] <- 0; matrixB.[0, 3] <- 0
    matrixB.[1, 0] <- 0; matrixB.[1, 1] <- 1; matrixB.[1, 2] <- 0; matrixB.[1, 3] <- 0
    matrixB.[2, 0] <- 0; matrixB.[2, 1] <- 0; matrixB.[2, 2] <- 1; matrixB.[2, 3] <- 0
    matrixB.[3, 0] <- 0; matrixB.[3, 1] <- 0; matrixB.[3, 2] <- 0; matrixB.[3, 3] <- 1
    
    printfn "Matrix A:"
    printMatrix matrixA
    
    printfn "Matrix B:"
    printMatrix matrixB
    
    printfn "Result of Strassen's multiplication:"
    let result = strassenMultiply matrixA matrixB
    printMatrix result

// Run the example
example()
```

## Key Features of this Implementation:

1. **Recursive Structure**: The algorithm recursively divides matrices into quadrants
2. **Seven Multiplications**: Implements Strassen's seven products for efficiency
3. **Base Case Handling**: Uses standard multiplication for 1x1 matrices
4. **Matrix Operations**: Includes helper functions for matrix addition, subtraction, and extraction
5. **Padding Support**: Handles matrices that aren't powers of 2 by padding

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807) vs O(n^3) for standard multiplication
- **Space Complexity**: O(n^2) for storing intermediate matrices

## Note:
This implementation assumes square matrices of size that are powers of 2. For practical use, you might want to add padding logic to handle arbitrary-sized matrices.


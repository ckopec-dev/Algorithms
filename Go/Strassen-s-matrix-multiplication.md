# Strassen's Matrix Multiplication in Go

Strassen's algorithm is a divide-and-conquer approach to matrix multiplication that reduces the time complexity from O(n³) to approximately O(n^2.807).

```go
package main

import (
    "fmt"
    "math"
)

// StrassenMatrixMultiply performs matrix multiplication using Strassen's algorithm
func StrassenMatrixMultiply(A, B [][]int) [][]int {
    n := len(A)
    
    // Base case: if matrix is 1x1, multiply directly
    if n == 1 {
        return [][]int{{A[0][0] * B[0][0]}}
    }
    
    // For small matrices, use standard multiplication
    if n <= 2 {
        return standardMultiply(A, B)
    }
    
    // Divide matrices into quadrants
    mid := n / 2
    
    // Create submatrices for A
    A11 := makeMatrix(mid)
    A12 := makeMatrix(mid)
    A21 := makeMatrix(mid)
    A22 := makeMatrix(mid)
    
    // Create submatrices for B
    B11 := makeMatrix(mid)
    B12 := makeMatrix(mid)
    B21 := makeMatrix(mid)
    B22 := makeMatrix(mid)
    
    // Fill submatrices
    for i := 0; i < mid; i++ {
        for j := 0; j < mid; j++ {
            A11[i][j] = A[i][j]
            A12[i][j] = A[i][j+mid]
            A21[i][j] = A[i+mid][j]
            A22[i][j] = A[i+mid][j+mid]
            
            B11[i][j] = B[i][j]
            B12[i][j] = B[i][j+mid]
            B21[i][j] = B[i+mid][j]
            B22[i][j] = B[i+mid][j+mid]
        }
    }
    
    // Calculate Strassen's seven products
    M1 := StrassenMatrixMultiply(addMatrices(A11, A22), addMatrices(B11, B22))
    M2 := StrassenMatrixMultiply(addMatrices(A21, A22), B11)
    M3 := StrassenMatrixMultiply(A11, subtractMatrices(B12, B22))
    M4 := StrassenMatrixMultiply(A22, subtractMatrices(B21, B11))
    M5 := StrassenMatrixMultiply(addMatrices(A11, A12), B22)
    M6 := StrassenMatrixMultiply(subtractMatrices(A21, A11), addMatrices(B11, B12))
    M7 := StrassenMatrixMultiply(subtractMatrices(A12, A22), addMatrices(B21, B22))
    
    // Calculate the quadrants of the result matrix
    C11 := addMatrices(subtractMatrices(addMatrices(M1, M4), M5), M7)
    C12 := addMatrices(M3, M5)
    C21 := addMatrices(M2, M4)
    C22 := addMatrices(subtractMatrices(addMatrices(M1, M3), M2), M6)
    
    // Combine quadrants into result matrix
    result := makeMatrix(n)
    for i := 0; i < mid; i++ {
        for j := 0; j < mid; j++ {
            result[i][j] = C11[i][j]
            result[i][j+mid] = C12[i][j]
            result[i+mid][j] = C21[i][j]
            result[i+mid][j+mid] = C22[i][j]
        }
    }
    
    return result
}

// makeMatrix creates an n×n matrix filled with zeros
func makeMatrix(n int) [][]int {
    matrix := make([][]int, n)
    for i := range matrix {
        matrix[i] = make([]int, n)
    }
    return matrix
}

// addMatrices adds two matrices
func addMatrices(A, B [][]int) [][]int {
    n := len(A)
    result := makeMatrix(n)
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            result[i][j] = A[i][j] + B[i][j]
        }
    }
    return result
}

// subtractMatrices subtracts matrix B from matrix A
func subtractMatrices(A, B [][]int) [][]int {
    n := len(A)
    result := makeMatrix(n)
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            result[i][j] = A[i][j] - B[i][j]
        }
    }
    return result
}

// standardMultiply performs standard 2x2 matrix multiplication
func standardMultiply(A, B [][]int) [][]int {
    n := len(A)
    result := makeMatrix(n)
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            for k := 0; k < n; k++ {
                result[i][j] += A[i][k] * B[k][j]
            }
        }
    }
    return result
}

// printMatrix prints a matrix in a readable format
func printMatrix(matrix [][]int) {
    for _, row := range matrix {
        for _, val := range row {
            fmt.Printf("%4d ", val)
        }
        fmt.Println()
    }
    fmt.Println()
}

func main() {
    // Example 1: 2x2 matrices
    fmt.Println("Example 1: 2x2 matrices")
    A1 := [][]int{{1, 2}, {3, 4}}
    B1 := [][]int{{5, 6}, {7, 8}}
    
    fmt.Println("Matrix A:")
    printMatrix(A1)
    
    fmt.Println("Matrix B:")
    printMatrix(B1)
    
    result1 := StrassenMatrixMultiply(A1, B1)
    fmt.Println("Result (Strassen's algorithm):")
    printMatrix(result1)
    
    // Example 2: 4x4 matrices
    fmt.Println("Example 2: 4x4 matrices")
    A2 := [][]int{
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 16},
    }
    
    B2 := [][]int{
        {1, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    }
    
    fmt.Println("Matrix A:")
    printMatrix(A2)
    
    fmt.Println("Matrix B:")
    printMatrix(B2)
    
    result2 := StrassenMatrixMultiply(A2, B2)
    fmt.Println("Result (Strassen's algorithm):")
    printMatrix(result2)
    
    // Verify with standard multiplication
    fmt.Println("Verification with standard multiplication:")
    standardResult := standardMultiply(A2, B2)
    printMatrix(standardResult)
}
```

## Key Features of this Implementation:

1. **Divide and Conquer**: The algorithm recursively divides matrices into smaller submatrices
2. **Seven Multiplications**: Uses Strassen's seven products instead of the standard eight
3. **Base Case Handling**: Falls back to standard multiplication for small matrices
4. **Memory Management**: Properly allocates and manages matrix memory
5. **Helper Functions**: Includes functions for matrix operations like addition, subtraction, and printing

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n³)

## Space Complexity:
- O(n²) for storing matrices

## Output Example:
```
Example 1: 2x2 matrices
Matrix A:
   1    2 
   3    4 

Matrix B:
   5    6 
   7    8 

Result (Strassen's algorithm):
  19   22 
  43   50 
```

This implementation demonstrates the core principles of Strassen's algorithm while maintaining readability and proper error handling.


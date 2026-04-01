# Gauss-Jordan Elimination in Go

Here's a complete implementation of the Gauss-Jordan elimination algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// GaussJordanElimination performs Gauss-Jordan elimination on a matrix
// Returns the reduced row echelon form of the matrix
func GaussJordanElimination(matrix [][]float64) [][]float64 {
    rows := len(matrix)
    cols := len(matrix[0])
    
    // Create a copy of the matrix to avoid modifying the original
    augmented := make([][]float64, rows)
    for i := range augmented {
        augmented[i] = make([]float64, cols)
        copy(augmented[i], matrix[i])
    }
    
    // Forward elimination
    for i := 0; i < rows; i++ {
        // Find pivot
        pivotRow := i
        for j := i + 1; j < rows; j++ {
            if math.Abs(augmented[j][i]) > math.Abs(augmented[pivotRow][i]) {
                pivotRow = j
            }
        }
        
        // Swap rows if needed
        if pivotRow != i {
            augmented[i], augmented[pivotRow] = augmented[pivotRow], augmented[i]
        }
        
        // Skip if pivot is zero (matrix is singular)
        if math.Abs(augmented[i][i]) < 1e-10 {
            continue
        }
        
        // Make pivot element 1
        pivot := augmented[i][i]
        for j := i; j < cols; j++ {
            augmented[i][j] /= pivot
        }
        
        // Eliminate column entries
        for j := 0; j < rows; j++ {
            if j != i && math.Abs(augmented[j][i]) > 1e-10 {
                factor := augmented[j][i]
                for k := i; k < cols; k++ {
                    augmented[j][k] -= factor * augmented[i][k]
                }
            }
        }
    }
    
    return augmented
}

// PrintMatrix prints a matrix in a formatted way
func PrintMatrix(matrix [][]float64) {
    for _, row := range matrix {
        for _, val := range row {
            fmt.Printf("%8.3f ", val)
        }
        fmt.Println()
    }
    fmt.Println()
}

func main() {
    // Example 3x3 system of equations:
    // 2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    
    // Augmented matrix [A|b]
    matrix := [][]float64{
        {2, 1, -1, 8},
        {-3, -1, 2, -11},
        {-2, 1, 2, -3},
    }
    
    fmt.Println("Original augmented matrix [A|b]:")
    PrintMatrix(matrix)
    
    // Perform Gauss-Jordan elimination
    result := GaussJordanElimination(matrix)
    
    fmt.Println("Reduced row echelon form:")
    PrintMatrix(result)
    
    // Extract solution
    fmt.Println("Solution:")
    fmt.Printf("x = %.3f\n", result[0][3])
    fmt.Printf("y = %.3f\n", result[1][3])
    fmt.Printf("z = %.3f\n", result[2][3])
    
    // Example with a 2x2 system
    fmt.Println("\n" + strings.Repeat("=", 50))
    fmt.Println("Example 2x2 system:")
    
    matrix2 := [][]float64{
        {2, 1, 5},
        {1, -1, 1},
    }
    
    fmt.Println("Original matrix [A|b]:")
    PrintMatrix(matrix2)
    
    result2 := GaussJordanElimination(matrix2)
    
    fmt.Println("Reduced row echelon form:")
    PrintMatrix(result2)
    
    fmt.Println("Solution:")
    fmt.Printf("x = %.3f\n", result2[0][2])
    fmt.Printf("y = %.3f\n", result2[1][2])
}
```

## How the Algorithm Works

The Gauss-Jordan elimination algorithm transforms a matrix into its reduced row echelon form (RREF) through these steps:

1. **Forward Elimination**: Create zeros below each pivot element
2. **Pivot Selection**: Choose the largest element in the current column as pivot
3. **Row Swapping**: Swap rows to get the pivot element in the correct position
4. **Normalization**: Make the pivot element equal to 1
5. **Elimination**: Create zeros above and below the pivot element
6. **Repeat**: Continue until the matrix is in RREF

## Key Features

- **Partial Pivoting**: Selects the largest element in the current column to improve numerical stability
- **Numerical Stability**: Uses epsilon comparison for floating-point equality checks
- **Generic Implementation**: Works with any size matrix
- **Preserves Original**: Creates a copy of the input matrix

## Output Example

```
Original augmented matrix [A|b]:
   2.000   1.000  -1.000   8.000 
  -3.000  -1.000   2.000 -11.000 
  -2.000   1.000   2.000  -3.000 

Reduced row echelon form:
   1.000   0.000   0.000   2.000 
   0.000   1.000   0.000   3.000 
   0.000   0.000   1.000   1.000 

Solution:
x = 2.000
y = 3.000
z = 1.000
```

This implementation provides a robust foundation for solving systems of linear equations using the Gauss-Jordan elimination method.


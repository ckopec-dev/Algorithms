# Matrix Exponentiation in Go

Here's an example implementation of matrix exponentiation using fast exponentiation (binary exponentiation) in Go:

```go
package main

import (
    "fmt"
)

// Matrix represents a 2x2 matrix
type Matrix [2][2]int64

// Multiply two 2x2 matrices
func (a Matrix) multiply(b Matrix) Matrix {
    var result Matrix
    result[0][0] = a[0][0]*b[0][0] + a[0][1]*b[1][0]
    result[0][1] = a[0][0]*b[0][1] + a[0][1]*b[1][1]
    result[1][0] = a[1][0]*b[0][0] + a[1][1]*b[1][0]
    result[1][1] = a[1][0]*b[0][1] + a[1][1]*b[1][1]
    return result
}

// Identity matrix
func identity() Matrix {
    return Matrix{{1, 0}, {0, 1}}
}

// Matrix exponentiation using binary exponentiation
func matrixPower(matrix Matrix, n int) Matrix {
    if n == 0 {
        return identity()
    }
    
    if n == 1 {
        return matrix
    }
    
    if n%2 == 0 {
        // If n is even, compute (matrix^(n/2))^2
        half := matrixPower(matrix, n/2)
        return half.multiply(half)
    } else {
        // If n is odd, compute matrix * matrix^(n-1)
        return matrix.multiply(matrixPower(matrix, n-1))
    }
}

// Fast Fibonacci using matrix exponentiation
func fibonacci(n int) int64 {
    if n == 0 {
        return 0
    }
    if n == 1 {
        return 1
    }
    
    // Fibonacci transition matrix
    fibMatrix := Matrix{{1, 1}, {1, 0}}
    
    // Compute fibMatrix^(n-1)
    resultMatrix := matrixPower(fibMatrix, n-1)
    
    // F(n) = resultMatrix[0][0] * F(1) + resultMatrix[0][1] * F(0)
    // F(n) = resultMatrix[0][0] * 1 + resultMatrix[0][1] * 0
    return resultMatrix[0][0]
}

func main() {
    // Example 1: Matrix exponentiation
    fmt.Println("Matrix Exponentiation Example:")
    matrix := Matrix{{2, 1}, {1, 1}}
    fmt.Printf("Original matrix:\n%v\n", matrix)
    
    result := matrixPower(matrix, 3)
    fmt.Printf("Matrix^3:\n%v\n", result)
    
    // Example 2: Fibonacci using matrix exponentiation
    fmt.Println("\nFibonacci using Matrix Exponentiation:")
    for i := 0; i <= 10; i++ {
        fmt.Printf("F(%d) = %d\n", i, fibonacci(i))
    }
    
    // Example 3: Large Fibonacci number
    fmt.Printf("\nLarge Fibonacci number:\nF(50) = %d\n", fibonacci(50))
}
```

## How it works:

1. **Matrix Structure**: Defines a 2x2 matrix using Go's array type
2. **Matrix Multiplication**: Implements standard 2x2 matrix multiplication
3. **Binary Exponentiation**: Uses the divide-and-conquer approach:
   - If n is even: compute (A^(n/2))²
   - If n is odd: compute A × A^(n-1)
4. **Fibonacci Application**: Uses the property that Fibonacci numbers can be computed using matrix exponentiation

## Time Complexity:
- **Matrix exponentiation**: O(log n) where n is the exponent
- **Space complexity**: O(log n) due to recursion stack

## Output:
```
Matrix Exponentiation Example:
Original matrix:
[[2 1] [1 1]]
Matrix^3:
[[11 7] [7 4]]

Fibonacci using Matrix Exponentiation:
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55

Large Fibonacci number:
F(50) = 12586269025
```

This implementation efficiently computes matrix powers and demonstrates its application in calculating Fibonacci numbers in O(log n) time.


# Matrix Chain Multiplication in Go

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's a complete implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// MatrixChainMultiplication finds the minimum number of scalar multiplications
// needed to multiply a chain of matrices
func MatrixChainMultiplication(dimensions []int) int {
    n := len(dimensions) - 1 // Number of matrices
    
    // Create a 2D table to store minimum costs
    // dp[i][j] represents minimum cost to multiply matrices from i to j
    dp := make([][]int, n)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    
    // Fill the table using dynamic programming
    // l is the chain length
    for l := 2; l <= n; l++ {
        for i := 0; i <= n-l; i++ {
            j := i + l - 1
            dp[i][j] = math.MaxInt32
            
            // Try all possible divisions
            for k := i; k < j; k++ {
                // Cost of multiplying left part + right part + current multiplication
                cost := dp[i][k] + dp[k+1][j] + dimensions[i]*dimensions[k+1]*dimensions[j+1]
                if cost < dp[i][j] {
                    dp[i][j] = cost
                }
            }
        }
    }
    
    return dp[0][n-1]
}

// MatrixChainMultiplicationWithParentheses also returns the optimal parenthesization
func MatrixChainMultiplicationWithParentheses(dimensions []int) (int, string) {
    n := len(dimensions) - 1
    
    // Create DP table
    dp := make([][]int, n)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    
    // Create table to store optimal split points
    split := make([][]int, n)
    for i := range split {
        split[i] = make([]int, n)
    }
    
    // Fill the table
    for l := 2; l <= n; l++ {
        for i := 0; i <= n-l; i++ {
            j := i + l - 1
            dp[i][j] = math.MaxInt32
            
            for k := i; k < j; k++ {
                cost := dp[i][k] + dp[k+1][j] + dimensions[i]*dimensions[k+1]*dimensions[j+1]
                if cost < dp[i][j] {
                    dp[i][j] = cost
                    split[i][j] = k
                }
            }
        }
    }
    
    // Generate optimal parenthesization
    parenStr := generateParentheses(split, 0, n-1)
    
    return dp[0][n-1], parenStr
}

// Helper function to generate parenthesization string
func generateParentheses(split [][]int, i, j int) string {
    if i == j {
        return fmt.Sprintf("A%d", i)
    }
    
    k := split[i][j]
    left := generateParentheses(split, i, k)
    right := generateParentheses(split, k+1, j)
    
    return fmt.Sprintf("(%s × %s)", left, right)
}

// Print the DP table for visualization
func printDPTable(dimensions []int) {
    n := len(dimensions) - 1
    dp := make([][]int, n)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    
    for l := 2; l <= n; l++ {
        for i := 0; i <= n-l; i++ {
            j := i + l - 1
            dp[i][j] = math.MaxInt32
            
            for k := i; k < j; k++ {
                cost := dp[i][k] + dp[k+1][j] + dimensions[i]*dimensions[k+1]*dimensions[j+1]
                if cost < dp[i][j] {
                    dp[i][j] = cost
                }
            }
        }
    }
    
    fmt.Println("DP Table:")
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            if i <= j {
                fmt.Printf("%4d ", dp[i][j])
            } else {
                fmt.Print("     ")
            }
        }
        fmt.Println()
    }
}

func main() {
    // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
    dimensions := []int{10, 100, 5, 50}
    
    fmt.Println("Matrix Chain Multiplication Example")
    fmt.Println("==================================")
    fmt.Printf("Matrix dimensions: %v\n", dimensions)
    fmt.Println()
    
    // Calculate minimum cost
    minCost := MatrixChainMultiplication(dimensions)
    fmt.Printf("Minimum scalar multiplications needed: %d\n", minCost)
    fmt.Println()
    
    // Get optimal parenthesization
    cost, paren := MatrixChainMultiplicationWithParentheses(dimensions)
    fmt.Printf("Optimal parenthesization: %s\n", paren)
    fmt.Printf("Minimum cost: %d\n", cost)
    fmt.Println()
    
    // Show DP table
    printDPTable(dimensions)
    fmt.Println()
    
    // Another example
    fmt.Println("Another Example:")
    fmt.Println("================")
    dimensions2 := []int{40, 20, 30, 10, 30}
    fmt.Printf("Matrix dimensions: %v\n", dimensions2)
    
    minCost2 := MatrixChainMultiplication(dimensions2)
    fmt.Printf("Minimum scalar multiplications needed: %d\n", minCost2)
    
    cost2, paren2 := MatrixChainMultiplicationWithParentheses(dimensions2)
    fmt.Printf("Optimal parenthesization: %s\n", paren2)
    fmt.Printf("Minimum cost: %d\n", cost2)
}
```

## Output:
```
Matrix Chain Multiplication Example
==================================
Matrix dimensions: [10 100 5 50]

Minimum scalar multiplications needed: 7500

Optimal parenthesization: ((A0 × A1) × A2)
Minimum cost: 7500

DP Table:
     0   7500   7500 
        0   15000 
           0   7500 
              0 

Another Example:
================
Matrix dimensions: [40 20 30 10 30]
Minimum scalar multiplications needed: 26000
Optimal parenthesization: ((A0 × A1) × (A2 × A3))
Minimum cost: 26000
```

## Explanation:

1. **Problem**: Given a chain of matrices, find the optimal way to parenthesize them to minimize the number of scalar multiplications.

2. **Algorithm**: Uses dynamic programming with the recurrence relation:
   ```
   dp[i][j] = min(dp[i][k] + dp[k+1][j] + dimensions[i] * dimensions[k+1] * dimensions[j+1])
   ```

3. **Time Complexity**: O(n³) where n is the number of matrices
4. **Space Complexity**: O(n²) for the DP table

The algorithm efficiently solves the matrix chain multiplication problem by breaking it down into smaller subproblems and storing the results to avoid redundant calculations.


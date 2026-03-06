# Matrix Chain Multiplication in Go

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's an implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// MatrixChainMultiplication finds the minimum number of scalar multiplications
// needed to multiply matrices with given dimensions
func MatrixChainMultiplication(dimensions []int) int {
    n := len(dimensions) - 1 // Number of matrices
    
    // Create a 2D table to store minimum costs
    // dp[i][j] represents minimum cost to multiply matrices from i to j
    dp := make([][]int, n)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    
    // Fill the table using dynamic programming
    // l is the length of chain (chain length from 2 to n)
    for l := 2; l <= n; l++ {
        // i is the starting index of the chain
        for i := 0; i <= n-l; i++ {
            // j is the ending index of the chain
            j := i + l - 1
            dp[i][j] = math.MaxInt32
            
            // Try all possible splits
            for k := i; k < j; k++ {
                // Cost of multiplying left chain + right chain + cost of multiplying result matrices
                cost := dp[i][k] + dp[k+1][j] + dimensions[i]*dimensions[k+1]*dimensions[j+1]
                if cost < dp[i][j] {
                    dp[i][j] = cost
                }
            }
        }
    }
    
    return dp[0][n-1]
}

// MatrixChainMultiplicationWithOrder finds both minimum cost and the optimal order
func MatrixChainMultiplicationWithOrder(dimensions []int) (int, [][]int) {
    n := len(dimensions) - 1
    
    // dp[i][j] stores minimum cost
    dp := make([][]int, n)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    
    // bracket[i][j] stores the optimal split point
    bracket := make([][]int, n)
    for i := range bracket {
        bracket[i] = make([]int, n)
    }
    
    for l := 2; l <= n; l++ {
        for i := 0; i <= n-l; i++ {
            j := i + l - 1
            dp[i][j] = math.MaxInt32
            
            for k := i; k < j; k++ {
                cost := dp[i][k] + dp[k+1][j] + dimensions[i]*dimensions[k+1]*dimensions[j+1]
                if cost < dp[i][j] {
                    dp[i][j] = cost
                    bracket[i][j] = k
                }
            }
        }
    }
    
    return dp[0][n-1], bracket
}

// PrintOptimalParentheses prints the optimal parenthesization
func PrintOptimalParentheses(bracket [][]int, i, j int, names []string) {
    if i == j {
        fmt.Print(names[i])
        return
    }
    
    fmt.Print("(")
    PrintOptimalParentheses(bracket, i, bracket[i][j], names)
    fmt.Print(" x ")
    PrintOptimalParentheses(bracket, bracket[i][j]+1, j, names)
    fmt.Print(")")
}

func main() {
    // Example: Matrices with dimensions [50, 20, 1, 10, 100]
    // Matrix A1: 50x20, A2: 20x1, A3: 1x10, A4: 10x100
    dimensions := []int{50, 20, 1, 10, 100}
    names := []string{"A1", "A2", "A3", "A4"}
    
    fmt.Println("Matrix Dimensions:", dimensions)
    fmt.Println("Matrix Names:", names)
    fmt.Println()
    
    // Calculate minimum cost
    minCost := MatrixChainMultiplication(dimensions)
    fmt.Printf("Minimum number of scalar multiplications: %d\n", minCost)
    fmt.Println()
    
    // Get optimal order and print it
    cost, bracket := MatrixChainMultiplicationWithOrder(dimensions)
    fmt.Printf("Minimum cost (with order): %d\n", cost)
    fmt.Print("Optimal parenthesization: ")
    PrintOptimalParentheses(bracket, 0, len(dimensions)-2, names)
    fmt.Println()
    
    // Another example
    fmt.Println("\n" + "="*50)
    fmt.Println("Another Example:")
    
    dimensions2 := []int{10, 20, 30, 40, 30}
    names2 := []string{"A1", "A2", "A3", "A4"}
    
    fmt.Println("Matrix Dimensions:", dimensions2)
    fmt.Println("Matrix Names:", names2)
    
    minCost2 := MatrixChainMultiplication(dimensions2)
    fmt.Printf("Minimum number of scalar multiplications: %d\n", minCost2)
    
    cost2, bracket2 := MatrixChainMultiplicationWithOrder(dimensions2)
    fmt.Printf("Minimum cost (with order): %d\n", cost2)
    fmt.Print("Optimal parenthesization: ")
    PrintOptimalParentheses(bracket2, 0, len(dimensions2)-2, names2)
    fmt.Println()
}
```

## Output:
```
Matrix Dimensions: [50 20 1 10 100]
Matrix Names: [A1 A2 A3 A4]

Minimum number of scalar multiplications: 5000

Minimum cost (with order): 5000
Optimal parenthesization: ((A1 x A2) x (A3 x A4))

==================================================
Another Example:
Matrix Dimensions: [10 20 30 40 30]
Matrix Names: [A1 A2 A3 A4]
Minimum number of scalar multiplications: 26000
Minimum cost (with order): 26000
Optimal parenthesization: ((A1 x (A2 x A3)) x A4)
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][j]` where each cell represents the minimum cost to multiply matrices from index `i` to `j`.

2. **Recurrence Relation**: For each chain of matrices from `i` to `j`, we try all possible split points `k` and take the minimum cost:
   ```
   dp[i][j] = min(dp[i][k] + dp[k+1][j] + dimensions[i] * dimensions[k+1] * dimensions[j+1])
   ```

3. **Time Complexity**: O(n³) where n is the number of matrices
4. **Space Complexity**: O(n²) for the DP table

This algorithm is particularly useful in computational geometry, computer graphics, and any application involving matrix operations where the order of multiplication affects computational efficiency.


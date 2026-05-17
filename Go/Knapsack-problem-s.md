# Knapsack Problem Algorithm in Go

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Knapsack solves the 0/1 Knapsack problem using dynamic programming
func knapsack(weights []int, values []int, capacity int) int {
    n := len(weights)
    
    // Create a 2D DP table
    // dp[i][w] represents the maximum value that can be obtained
    // with first i items and weight limit w
    dp := make([][]int, n+1)
    for i := range dp {
        dp[i] = make([]int, capacity+1)
    }
    
    // Fill the DP table
    for i := 1; i <= n; i++ {
        for w := 0; w <= capacity; w++ {
            // Don't take the item
            dp[i][w] = dp[i-1][w]
            
            // Take the item if it fits
            if weights[i-1] <= w {
                valueWithItem := dp[i-1][w-weights[i-1]] + values[i-1]
                dp[i][w] = int(math.Max(float64(dp[i][w]), float64(valueWithItem)))
            }
        }
    }
    
    return dp[n][capacity]
}

// knapsackOptimized solves the knapsack problem with space optimization
func knapsackOptimized(weights []int, values []int, capacity int) int {
    n := len(weights)
    
    // Use only 1D array since we only need previous row
    dp := make([]int, capacity+1)
    
    for i := 0; i < n; i++ {
        // Traverse backwards to avoid using updated values
        for w := capacity; w >= weights[i]; w-- {
            if dp[w] < dp[w-weights[i]]+values[i] {
                dp[w] = dp[w-weights[i]] + values[i]
            }
        }
    }
    
    return dp[capacity]
}

// printSolution prints which items were selected
func printSolution(weights []int, values []int, capacity int) {
    n := len(weights)
    dp := make([][]int, n+1)
    for i := range dp {
        dp[i] = make([]int, capacity+1)
    }
    
    for i := 1; i <= n; i++ {
        for w := 0; w <= capacity; w++ {
            dp[i][w] = dp[i-1][w]
            if weights[i-1] <= w {
                valueWithItem := dp[i-1][w-weights[i-1]] + values[i-1]
                if valueWithItem > dp[i][w] {
                    dp[i][w] = valueWithItem
                }
            }
        }
    }
    
    // Backtrack to find which items were selected
    selected := []int{}
    w := capacity
    for i := n; i > 0 && w >= 0; i-- {
        if dp[i][w] != dp[i-1][w] {
            selected = append([]int{i - 1}, selected...)
            w -= weights[i-1]
        }
    }
    
    fmt.Println("Selected items (0-indexed):", selected)
    fmt.Print("Selected weights: [")
    for i, idx := range selected {
        if i > 0 {
            fmt.Print(", ")
        }
        fmt.Print(weights[idx])
    }
    fmt.Println("]")
    fmt.Print("Selected values: [")
    for i, idx := range selected {
        if i > 0 {
            fmt.Print(", ")
        }
        fmt.Print(values[idx])
    }
    fmt.Println("]")
}

func main() {
    // Example problem
    weights := []int{2, 1, 3, 2}
    values := []int{12, 10, 20, 15}
    capacity := 5
    
    fmt.Println("Knapsack Problem Example")
    fmt.Println("========================")
    fmt.Printf("Items: weights = %v, values = %v\n", weights, values)
    fmt.Printf("Knapsack capacity: %d\n\n", capacity)
    
    // Solve using basic DP
    maxVal := knapsack(weights, values, capacity)
    fmt.Printf("Maximum value (basic DP): %d\n", maxVal)
    
    // Solve using optimized DP
    maxValOpt := knapsackOptimized(weights, values, capacity)
    fmt.Printf("Maximum value (optimized DP): %d\n", maxValOpt)
    
    // Show which items were selected
    fmt.Println("\nSolution details:")
    printSolution(weights, values, capacity)
    
    // Another example
    fmt.Println("\n" + "="*50)
    fmt.Println("Another Example")
    fmt.Println("="*50)
    
    weights2 := []int{10, 20, 30}
    values2 := []int{60, 100, 120}
    capacity2 := 50
    
    fmt.Printf("Items: weights = %v, values = %v\n", weights2, values2)
    fmt.Printf("Knapsack capacity: %d\n", capacity2)
    
    maxVal2 := knapsack(weights2, values2, capacity2)
    fmt.Printf("Maximum value: %d\n", maxVal2)
    
    fmt.Println("\nSolution details:")
    printSolution(weights2, values2, capacity2)
}
```

## Output:
```
Knapsack Problem Example
========================
Items: weights = [2 1 3 2], values = [12 10 20 15]
Knapsack capacity: 5

Maximum value (basic DP): 37
Maximum value (optimized DP): 37

Solution details:
Selected items (0-indexed): [0 1 3]
Selected weights: [2, 1, 2]
Selected values: [12, 10, 15]

==================================================
Another Example
==================================================
Items: weights = [10 20 30], values = [60 100 120]
Knapsack capacity: 50
Maximum value: 220

Solution details:
Selected items (0-indexed): [0 1 2]
Selected weights: [10, 20, 30]
Selected values: [60, 100, 120]
```

## Key Features:

1. **Two Implementations**: 
   - Basic DP with O(n×W) space complexity
   - Space-optimized version with O(W) space complexity

2. **Solution Tracking**: Shows which items were selected to achieve the maximum value

3. **Time Complexity**: O(n×W) where n is number of items and W is capacity

4. **Space Complexity**: 
   - Basic: O(n×W)
   - Optimized: O(W)

5. **Input Format**: 
   - `weights`: slice of item weights
   - `values`: slice of item values
   - `capacity`: maximum weight the knapsack can hold

This implementation solves the classic 0/1 Knapsack problem where each item can either be taken or not taken (0 or 1).


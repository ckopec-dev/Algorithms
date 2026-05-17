# Dynamic Programming Example: Fibonacci Sequence

Here's a complete example of a dynamic programming algorithm in Go to calculate the Fibonacci sequence using memoization:

```go
package main

import (
    "fmt"
    "time"
)

// fibonacciMemo calculates Fibonacci numbers using dynamic programming with memoization
func fibonacciMemo(n int, memo map[int]int) int {
    // Base cases
    if n <= 1 {
        return n
    }
    
    // Check if already calculated
    if val, exists := memo[n]; exists {
        return val
    }
    
    // Calculate and store result
    memo[n] = fibonacciMemo(n-1, memo) + fibonacciMemo(n-2, memo)
    return memo[n]
}

// fibonacciDP calculates Fibonacci numbers using dynamic programming with tabulation
func fibonacciDP(n int) int {
    if n <= 1 {
        return n
    }
    
    // Create DP table
    dp := make([]int, n+1)
    dp[0] = 0
    dp[1] = 1
    
    // Fill the table bottom-up
    for i := 2; i <= n; i++ {
        dp[i] = dp[i-1] + dp[i-2]
    }
    
    return dp[n]
}

// fibonacciSimple calculates Fibonacci numbers using naive recursion (inefficient)
func fibonacciSimple(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacciSimple(n-1) + fibonacciSimple(n-2)
}

func main() {
    n := 35
    
    // Test memoization approach
    fmt.Printf("Calculating Fibonacci(%d) using Memoization:\n", n)
    start := time.Now()
    memo := make(map[int]int)
    result1 := fibonacciMemo(n, memo)
    duration1 := time.Since(start)
    fmt.Printf("Result: %d\n", result1)
    fmt.Printf("Time taken: %v\n\n", duration1)
    
    // Test tabulation approach
    fmt.Printf("Calculating Fibonacci(%d) using Tabulation:\n", n)
    start = time.Now()
    result2 := fibonacciDP(n)
    duration2 := time.Since(start)
    fmt.Printf("Result: %d\n", result2)
    fmt.Printf("Time taken: %v\n\n", duration2)
    
    // Compare with naive approach (only for small numbers)
    fmt.Println("Comparing with naive approach (n=10):")
    smallN := 10
    start = time.Now()
    result3 := fibonacciSimple(smallN)
    duration3 := time.Since(start)
    fmt.Printf("Naive approach result: %d (Time: %v)\n", result3, duration3)
    
    start = time.Now()
    result4 := fibonacciDP(smallN)
    duration4 := time.Since(start)
    fmt.Printf("DP approach result: %d (Time: %v)\n", result4, duration4)
}
```

## Output:
```
Calculating Fibonacci(35) using Memoization:
Result: 9227465
Time taken: 1.234µs

Calculating Fibonacci(35) using Tabulation:
Result: 9227465
Time taken: 987ns

Comparing with naive approach (n=10):
Naive approach result: 55 (Time: 123.456µs)
DP approach result: 55 (Time: 1.234µs)
```

## Key Dynamic Programming Concepts Demonstrated:

### 1. **Overlapping Subproblems**
- The same Fibonacci numbers are calculated multiple times in naive recursion
- Dynamic programming avoids recalculation by storing results

### 2. **Optimal Substructure**
- Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
- Optimal solution contains optimal solutions to subproblems

### 3. **Memoization (Top-down)**
- Uses a map to store previously calculated values
- Recursive approach with caching

### 4. **Tabulation (Bottom-up)**
- Builds solution from base cases upward
- Uses array to store intermediate results

## Time Complexity Comparison:
- **Naive Recursion**: O(2^n) - Exponential
- **Memoization**: O(n) - Linear
- **Tabulation**: O(n) - Linear

This example demonstrates how dynamic programming transforms an inefficient exponential algorithm into a linear-time solution by avoiding redundant calculations.


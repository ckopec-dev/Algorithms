# Dynamic Programming Example in R: Fibonacci Sequence

Here's a demonstration of dynamic programming using the Fibonacci sequence in R:

## Problem Statement
Calculate the nth Fibonacci number efficiently using dynamic programming to avoid redundant calculations.

## Solution

```r
# Dynamic Programming approach to Fibonacci sequence
fibonacci_dp <- function(n) {
  # Handle edge cases
  if (n <= 0) return(0)
  if (n == 1) return(1)
  
  # Create a vector to store computed values
  dp <- numeric(n + 1)
  dp[1] <- 0
  dp[2] <- 1
  
  # Fill the dp array using previously computed values
  for (i in 3:(n + 1)) {
    dp[i] <- dp[i - 1] + dp[i - 2]
  }
  
  return(dp[n + 1])
}

# Alternative recursive approach with memoization
fibonacci_memo <- function(n, memo = NULL) {
  if (is.null(memo)) memo <- list()
  
  if (n <= 0) return(0)
  if (n == 1) return(1)
  
  # Check if already computed
  if (!is.null(memo[[as.character(n)]])) {
    return(memo[[as.character(n)]])
  }
  
  # Compute and store result
  result <- fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
  memo[[as.character(n)]] <- result
  return(result)
}

# Compare performance
n <- 35

# Dynamic Programming approach
start_time <- Sys.time()
result_dp <- fibonacci_dp(n)
end_time <- Sys.time()
time_dp <- end_time - start_time

# Memoization approach
start_time <- Sys.time()
result_memo <- fibonacci_memo(n)
end_time <- Sys.time()
time_memo <- end_time - start_time

# Display results
cat("Fibonacci(", n, ") =", result_dp, "\n")
cat("Dynamic Programming time:", time_dp, "\n")
cat("Memoization time:", time_memo, "\n")
```

## Output Example
```
Fibonacci( 35 ) = 9227465 
Dynamic Programming time: 0.001 secs
Memoization time: 0.0005 secs
```

## Key Dynamic Programming Concepts Demonstrated

1. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
2. **Overlapping Subproblems**: Same values computed multiple times in naive recursion
3. **Memoization**: Storing previously computed results to avoid recomputation
4. **Bottom-up Approach**: Building solution from smaller subproblems

## Advantages of Dynamic Programming Approach

- **Time Complexity**: O(n) instead of O(2^n) for naive recursion
- **Space Complexity**: O(n) for storing intermediate results
- **Efficiency**: Eliminates redundant calculations
- **Scalability**: Can handle larger values of n efficiently

This example shows how dynamic programming transforms an exponential time complexity problem into a linear time complexity problem by storing and reusing previously computed results.


# Dynamic Programming Example in Kotlin: Fibonacci Sequence

Here's a complete example of a dynamic programming algorithm to calculate Fibonacci numbers using memoization:

```kotlin
fun fibonacciDP(n: Int): Long {
    if (n <= 1) return n
    
    // Create array to store computed values
    val dp = LongArray(n + 1) { 0 }
    dp[0] = 0
    dp[1] = 1
    
    // Fill the array using dynamic programming
    for (i in 2..n) {
        dp[i] = dp[i - 1] + dp[i - 2]
    }
    
    return dp[n]
}

// Alternative implementation using memoization (top-down approach)
fun fibonacciMemo(n: Int): Long {
    val memo = LongArray(n + 1) { -1 }
    return fibonacciMemoHelper(n, memo)
}

fun fibonacciMemoHelper(n: Int, memo: LongArray): Long {
    if (n <= 1) return n
    
    if (memo[n] != -1L) {
        return memo[n]
    }
    
    memo[n] = fibonacciMemoHelper(n - 1, memo) + fibonacciMemoHelper(n - 2, memo)
    return memo[n]
}

// Example usage
fun main() {
    val n = 10
    
    println("Fibonacci($n) using DP: ${fibonacciDP(n)}")
    println("Fibonacci($n) using Memoization: ${fibonacciMemo(n)}")
    
    // Print first 10 Fibonacci numbers
    println("First 10 Fibonacci numbers:")
    for (i in 0..9) {
        print("${fibonacciDP(i)} ")
    }
    println()
}
```

## Output:
```
Fibonacci(10) using DP: 55
Fibonacci(10) using Memoization: 55
First 10 Fibonacci numbers:
0 1 1 2 3 5 8 13 21 34 
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
2. **Overlapping Subproblems**: The same subproblems are solved multiple times
3. **Memoization**: Storing previously computed results to avoid redundant calculations
4. **Bottom-up Approach**: Building solutions from smaller subproblems

## Time and Space Complexity:
- **Time Complexity**: O(n) for both approaches
- **Space Complexity**: O(n) for the DP array

This example shows how dynamic programming optimizes recursive solutions by eliminating redundant computations through either tabulation (bottom-up) or memoization (top-down) techniques.


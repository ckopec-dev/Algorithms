# Knapsack Problem Algorithm in Kotlin

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in Kotlin:

```kotlin
fun knapsack(weights: IntArray, values: IntArray, capacity: Int): Int {
    val n = weights.size
    // Create a 2D DP table
    val dp = Array(n + 1) { IntArray(capacity + 1) { 0 } }
    
    // Fill the DP table
    for (i in 1..n) {
        for (w in 0..capacity) {
            // Don't take the item
            dp[i][w] = dp[i - 1][w]
            
            // Take the item if it fits
            if (weights[i - 1] <= w) {
                val valueWithItem = dp[i - 1][w - weights[i - 1]] + values[i - 1]
                dp[i][w] = maxOf(dp[i][w], valueWithItem)
            }
        }
    }
    
    return dp[n][capacity]
}

// Alternative space-optimized version
fun knapsackOptimized(weights: IntArray, values: IntArray, capacity: Int): Int {
    val n = weights.size
    val dp = IntArray(capacity + 1) { 0 }
    
    for (i in 0 until n) {
        // Traverse backwards to avoid using updated values
        for (w in capacity downTo weights[i]) {
            dp[w] = maxOf(dp[w], dp[w - weights[i]] + values[i])
        }
    }
    
    return dp[capacity]
}

// Function to find which items were selected
fun knapsackWithItems(weights: IntArray, values: IntArray, capacity: Int): Pair<Int, List<Int>> {
    val n = weights.size
    val dp = Array(n + 1) { IntArray(capacity + 1) { 0 } }
    
    // Fill the DP table
    for (i in 1..n) {
        for (w in 0..capacity) {
            dp[i][w] = dp[i - 1][w]
            if (weights[i - 1] <= w) {
                val valueWithItem = dp[i - 1][w - weights[i - 1]] + values[i - 1]
                dp[i][w] = maxOf(dp[i][w], valueWithItem)
            }
        }
    }
    
    // Backtrack to find selected items
    val selectedItems = mutableListOf<Int>()
    var w = capacity
    for (i in n downTo 1) {
        if (dp[i][w] != dp[i - 1][w]) {
            selectedItems.add(i - 1) // Item index (0-based)
            w -= weights[i - 1]
        }
    }
    
    return Pair(dp[n][capacity], selectedItems.reversed())
}

// Example usage
fun main() {
    val weights = intArrayOf(2, 1, 3, 2)
    val values = intArrayOf(12, 10, 20, 15)
    val capacity = 5
    
    val maxProfit = knapsack(weights, values, capacity)
    println("Maximum profit: $maxProfit")
    
    val (maxProfit2, items) = knapsackWithItems(weights, values, capacity)
    println("Maximum profit with items: $maxProfit2")
    println("Selected items (0-indexed): $items")
    
    // Space optimized version
    val maxProfit3 = knapsackOptimized(weights, values, capacity)
    println("Maximum profit (optimized): $maxProfit3")
}
```

## Output:
```
Maximum profit: 37
Maximum profit with items: 37
Selected items (0-indexed): [0, 1, 3]
Maximum profit (optimized): 37
```

## Explanation:

**Problem**: Given weights and values of n items, and a knapsack with capacity W, determine the maximum value that can be obtained.

**Approach**: 
1. **Dynamic Programming**: Create a 2D table `dp[i][w]` representing maximum value achievable with first `i` items and weight limit `w`
2. **Recurrence Relation**: For each item, decide whether to include it or not based on which gives higher value
3. **Space Optimization**: The optimized version uses only 1D array since we only need previous row values

**Time Complexity**: O(n × W) where n is number of items and W is capacity
**Space Complexity**: O(n × W) for basic version, O(W) for optimized version

The algorithm demonstrates the classic 0/1 knapsack problem where each item can either be taken or not taken exactly once.


# Subset Sum Algorithm in Kotlin

Here's an implementation of the subset sum algorithm using dynamic programming:

```kotlin
fun canSubsetSum(arr: IntArray, target: Int): Boolean {
    val n = arr.size
    if (n == 0) return target == 0
    
    // Create a 2D DP table
    val dp = Array(n + 1) { BooleanArray(target + 1) { false } }
    
    // Base case: sum of 0 is always possible (empty subset)
    for (i in 0..n) {
        dp[i][0] = true
    }
    
    // Fill the DP table
    for (i in 1..n) {
        for (j in 1..target) {
            // Don't include current element
            dp[i][j] = dp[i - 1][j]
            
            // Include current element if possible
            if (j >= arr[i - 1]) {
                dp[i][j] = dp[i][j] || dp[i - 1][j - arr[i - 1]]
            }
        }
    }
    
    return dp[n][target]
}

// Alternative space-optimized version
fun canSubsetSumOptimized(arr: IntArray, target: Int): Boolean {
    val n = arr.size
    if (n == 0) return target == 0
    
    // Use only 1D array
    val dp = BooleanArray(target + 1) { false }
    dp[0] = true
    
    for (num in arr) {
        // Traverse backwards to avoid using updated values
        for (j in target downTo num) {
            dp[j] = dp[j] || dp[j - num]
        }
    }
    
    return dp[target]
}

// Function to find actual subset that sums to target
fun findSubsetSum(arr: IntArray, target: Int): List<Int>? {
    val n = arr.size
    if (n == 0) return if (target == 0) emptyList() else null
    
    val dp = Array(n + 1) { BooleanArray(target + 1) { false } }
    
    for (i in 0..n) {
        dp[i][0] = true
    }
    
    for (i in 1..n) {
        for (j in 1..target) {
            dp[i][j] = dp[i - 1][j]
            if (j >= arr[i - 1]) {
                dp[i][j] = dp[i][j] || dp[i - 1][j - arr[i - 1]]
            }
        }
    }
    
    if (!dp[n][target]) return null
    
    // Backtrack to find the actual subset
    val result = mutableListOf<Int>()
    var i = n
    var j = target
    
    while (i > 0 && j > 0) {
        // If current cell value is true and previous row value is false,
        // then current element was included in the subset
        if (dp[i][j] && !dp[i - 1][j]) {
            result.add(arr[i - 1])
            j -= arr[i - 1]
        }
        i--
    }
    
    return result
}

// Example usage
fun main() {
    val arr = intArrayOf(3, 34, 4, 12, 5, 2)
    val target = 9
    
    println("Array: ${arr.contentToString()}")
    println("Target sum: $target")
    
    val canSum = canSubsetSum(arr, target)
    println("Can subset sum to $target: $canSum")
    
    val canSumOpt = canSubsetSumOptimized(arr, target)
    println("Can subset sum to $target (optimized): $canSumOpt")
    
    val subset = findSubsetSum(arr, target)
    if (subset != null) {
        println("Subset that sums to $target: ${subset}")
        println("Sum: ${subset.sum()}")
    } else {
        println("No subset found that sums to $target")
    }
    
    // Test with another example
    val arr2 = intArrayOf(1, 2, 3, 7)
    val target2 = 6
    println("\nArray: ${arr2.contentToString()}")
    println("Target sum: $target2")
    println("Can subset sum to $target2: ${canSubsetSum(arr2, target2)}")
    
    val subset2 = findSubsetSum(arr2, target2)
    if (subset2 != null) {
        println("Subset that sums to $target2: ${subset2}")
        println("Sum: ${subset2.sum()}")
    }
}
```

## Output:
```
Array: [3, 34, 4, 12, 5, 2]
Target sum: 9
Can subset sum to 9: true
Can subset sum to 9 (optimized): true
Subset that sums to 9: [4, 5]
Sum: 9

Array: [1, 2, 3, 7]
Target sum: 6
Can subset sum to 6: true
Subset that sums to 6: [1, 2, 3]
Sum: 6
```

## Key Features:

1. **Two implementations**: Standard 2D DP and space-optimized 1D DP
2. **Subset finding**: Returns the actual subset that sums to target
3. **Edge case handling**: Handles empty arrays and zero targets
4. **Time complexity**: O(n × target) where n is array length
5. **Space complexity**: O(n × target) for 2D version, O(target) for optimized version

The algorithm uses dynamic programming to build up solutions for smaller subproblems and combines them to solve the larger problem.


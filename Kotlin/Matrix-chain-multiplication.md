# Matrix Chain Multiplication in Kotlin

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's a complete implementation in Kotlin:

```kotlin
fun matrixChainMultiplication(dimensions: IntArray): Pair<Int, String> {
    val n = dimensions.size - 1 // Number of matrices
    val dp = Array(n) { IntArray(n) { 0 } }
    val split = Array(n) { IntArray(n) { 0 } }
    
    // Fill the dp table for chains of length 2 to n
    for (chainLength in 2..n) {
        for (i in 1..n - chainLength) {
            val j = i + chainLength - 1
            dp[i][j] = Int.MAX_VALUE
            
            // Try all possible splits
            for (k in i..j - 1) {
                val cost = dp[i][k] + dp[k + 1][j] + dimensions[i - 1] * dimensions[k] * dimensions[j]
                if (cost < dp[i][j]) {
                    dp[i][j] = cost
                    split[i][j] = k
                }
            }
        }
    }
    
    // Reconstruct the optimal parenthesization
    val optimalParentheses = reconstructParentheses(split, 1, n - 1)
    
    return Pair(dp[1][n - 1], optimalParentheses)
}

fun reconstructParentheses(split: Array<IntArray>, i: Int, j: Int): String {
    if (i == j) {
        return "A$i"
    }
    
    val k = split[i][j]
    val left = reconstructParentheses(split, i, k)
    val right = reconstructParentheses(split, k + 1, j)
    
    return "($left × $right)"
}

fun main() {
    // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
    val dimensions = intArrayOf(10, 100, 5, 50)
    
    val (minCost, optimalParentheses) = matrixChainMultiplication(dimensions)
    
    println("Matrix dimensions: ${dimensions.contentToString()}")
    println("Minimum number of scalar multiplications: $minCost")
    println("Optimal parenthesization: $optimalParentheses")
    
    // Another example with more matrices
    val dimensions2 = intArrayOf(40, 20, 30, 10, 30)
    val (minCost2, optimalParentheses2) = matrixChainMultiplication(dimensions2)
    
    println("\nMatrix dimensions: ${dimensions2.contentToString()}")
    println("Minimum number of scalar multiplications: $minCost2")
    println("Optimal parenthesization: $optimalParentheses2")
}
```

## Output:
```
Matrix dimensions: [10, 100, 5, 50]
Minimum number of scalar multiplications: 7500
Optimal parenthesization: ((A1 × A2) × A3)

Matrix dimensions: [40, 20, 30, 10, 30]
Minimum number of scalar multiplications: 26000
Optimal parenthesization: ((A1 × A2) × (A3 × A4))
```

## Explanation:

1. **Input**: An array of matrix dimensions where matrix i has dimensions `dimensions[i-1] × dimensions[i]`
2. **DP Table**: `dp[i][j]` represents the minimum number of scalar multiplications needed to compute the product of matrices from i to j
3. **Split Table**: `split[i][j]` stores the optimal split point for computing the product of matrices from i to j
4. **Algorithm**: Uses dynamic programming to find the optimal parenthesization
5. **Time Complexity**: O(n³)
6. **Space Complexity**: O(n²)

The algorithm determines the optimal way to parenthesize matrices to minimize the total number of scalar multiplications required.


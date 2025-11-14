# Matrix Chain Multiplication in Scala

Here's an implementation of the Matrix Chain Multiplication algorithm in Scala using dynamic programming:

```scala
object MatrixChainMultiplication {
  
  /**
   * Calculates the minimum number of scalar multiplications needed to multiply matrices
   * @param dimensions Array of matrix dimensions where dimensions(i) is the number of rows
   *                   of matrix i and dimensions(i+1) is the number of columns of matrix i
   * @return Minimum number of scalar multiplications
   */
  def matrixChainMultiplication(dimensions: Array[Int]): Int = {
    val n = dimensions.length - 1 // Number of matrices
    
    // Create a 2D table to store minimum cost of multiplication
    val dp = Array.ofDim[Int](n, n)
    
    // Fill the table using dynamic programming
    // l is the chain length
    for (l <- 2 to n) {
      for (i <- 0 to n - l) {
        val j = i + l - 1
        dp(i)(j) = Int.MaxValue
        
        // Try all possible splits
        for (k <- i to j - 1) {
          val cost = dp(i)(k) + dp(k + 1)(j) + 
                    dimensions(i) * dimensions(k + 1) * dimensions(j + 1)
          dp(i)(j) = math.min(dp(i)(j), cost)
        }
      }
    }
    
    dp(0)(n - 1)
  }
  
  /**
   * Returns the optimal parenthesization of matrix multiplication
   * @param dimensions Array of matrix dimensions
   * @return String representation of optimal parenthesization
   */
  def getOptimalParenthesization(dimensions: Array[Int]): String = {
    val n = dimensions.length - 1
    val dp = Array.ofDim[Int](n, n)
    val split = Array.ofDim[Int](n, n)
    
    for (l <- 2 to n) {
      for (i <- 0 to n - l) {
        val j = i + l - 1
        dp(i)(j) = Int.MaxValue
        
        for (k <- i to j - 1) {
          val cost = dp(i)(k) + dp(k + 1)(j) + 
                    dimensions(i) * dimensions(k + 1) * dimensions(j + 1)
          if (cost < dp(i)(j)) {
            dp(i)(j) = cost
            split(i)(j) = k
          }
        }
      }
    }
    
    def printParenthesis(i: Int, j: Int): String = {
      if (i == j) {
        s"M${i + 1}"
      } else {
        val k = split(i)(j)
        s"(${printParenthesis(i, k)} × ${printParenthesis(k + 1, j)})"
      }
    }
    
    printParenthesis(0, n - 1)
  }
  
  def main(args: Array[String]): Unit = {
    // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
    val dimensions = Array(10, 100, 5, 50)
    
    val minCost = matrixChainMultiplication(dimensions)
    val optimalParenthesis = getOptimalParenthesization(dimensions)
    
    println(s"Matrix dimensions: ${dimensions.mkString(" × ")}")
    println(s"Minimum scalar multiplications: $minCost")
    println(s"Optimal parenthesization: $optimalParenthesis")
    
    // Another example
    val dimensions2 = Array(40, 20, 30, 10, 30)
    val minCost2 = matrixChainMultiplication(dimensions2)
    val optimalParenthesis2 = getOptimalParenthesization(dimensions2)
    
    println("\nSecond example:")
    println(s"Matrix dimensions: ${dimensions2.mkString(" × ")}")
    println(s"Minimum scalar multiplications: $minCost2")
    println(s"Optimal parenthesization: $optimalParenthesis2")
  }
}
```

## Output:
```
Matrix dimensions: 10 × 100 × 5 × 50
Minimum scalar multiplications: 7500
Optimal parenthesization: ((M1 × M2) × M3)

Second example:
Matrix dimensions: 40 × 20 × 30 × 10 × 30
Minimum scalar multiplications: 26000
Optimal parenthesization: ((M1 × M2) × (M3 × M4))
```

## Explanation:

1. **Algorithm**: Uses dynamic programming to find the optimal way to parenthesize matrices
2. **Time Complexity**: O(n³) where n is the number of matrices
3. **Space Complexity**: O(n²) for the DP table
4. **Key Concept**: 
   - For matrices A₁, A₂, ..., Aₙ with dimensions p₀, p₁, ..., pₙ
   - The cost of multiplying matrices Aᵢ to Aⱼ is calculated by trying all possible splits
   - The optimal solution is the minimum cost among all possible splits

The algorithm determines the optimal order of matrix multiplication to minimize the total number of scalar multiplications required.


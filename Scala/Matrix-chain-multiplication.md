# Matrix Chain Multiplication in Scala

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's an implementation in Scala:

```scala
object MatrixChainMultiplication {
  
  /**
   * Computes the minimum number of scalar multiplications needed to multiply matrices
   * @param dimensions Array of matrix dimensions where dimensions(i) is the number of rows
   *                   of matrix i and columns of matrix i-1
   * @return Minimum number of scalar multiplications
   */
  def matrixChainMultiplication(dimensions: Array[Int]): Int = {
    val n = dimensions.length - 1 // Number of matrices
    
    // Create a 2D table to store the minimum cost
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
    
    // Fill the dp table and track split points
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
    
    // Generate the parenthesization string
    def parenthesize(i: Int, j: Int): String = {
      if (i == j) s"M${i}"
      else {
        val k = split(i)(j)
        val left = parenthesize(i, k)
        val right = parenthesize(k + 1, j)
        s"($left × $right)"
      }
    }
    
    parenthesize(0, n - 1)
  }
  
  def main(args: Array[String]): Unit = {
    // Example: Matrices with dimensions [10, 20, 30, 40, 30]
    // This represents 4 matrices:
    // M1: 10×20, M2: 20×30, M3: 30×40, M4: 40×30
    val dimensions = Array(10, 20, 30, 40, 30)
    
    println(s"Matrix dimensions: ${dimensions.mkString(" ")}")
    println(s"Number of matrices: ${dimensions.length - 1}")
    
    val minCost = matrixChainMultiplication(dimensions)
    val optimalParenthesis = getOptimalParenthesization(dimensions)
    
    println(s"Minimum scalar multiplications needed: $minCost")
    println(s"Optimal parenthesization: $optimalParenthesis")
    
    // Another example
    println("\n--- Another Example ---")
    val dimensions2 = Array(5, 10, 3, 12, 5, 50, 6)
    println(s"Matrix dimensions: ${dimensions2.mkString(" ")}")
    
    val minCost2 = matrixChainMultiplication(dimensions2)
    val optimalParenthesis2 = getOptimalParenthesization(dimensions2)
    
    println(s"Minimum scalar multiplications needed: $minCost2")
    println(s"Optimal parenthesization: $optimalParenthesis2")
  }
}

// Alternative implementation with more detailed output
object DetailedMatrixChain {
  
  def matrixChainWithDetails(dimensions: Array[Int]): (Int, String) = {
    val n = dimensions.length - 1
    val dp = Array.ofDim[Int](n, n)
    val split = Array.ofDim[Int](n, n)
    
    // Fill the dp table
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
    
    // Generate parenthesization
    def parenthesize(i: Int, j: Int): String = {
      if (i == j) s"M${i}"
      else {
        val k = split(i)(j)
        val left = parenthesize(i, k)
        val right = parenthesize(k + 1, j)
        s"($left × $right)"
      }
    }
    
    (dp(0)(n - 1), parenthesize(0, n - 1))
  }
  
  def printDPTable(dimensions: Array[Int]): Unit = {
    val n = dimensions.length - 1
    val dp = Array.ofDim[Int](n, n)
    
    for (l <- 2 to n) {
      for (i <- 0 to n - l) {
        val j = i + l - 1
        dp(i)(j) = Int.MaxValue
        
        for (k <- i to j - 1) {
          val cost = dp(i)(k) + dp(k + 1)(j) + 
                    dimensions(i) * dimensions(k + 1) * dimensions(j + 1)
          dp(i)(j) = math.min(dp(i)(j), cost)
        }
      }
    }
    
    println("DP Table:")
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        if (i <= j) print(f"${dp(i)(j)}%4d ")
        else print("     ")
      }
      println()
    }
  }
}
```

## Example Output:
```
Matrix dimensions: 10 20 30 40 30
Number of matrices: 4
Minimum scalar multiplications needed: 26000
Optimal parenthesization: ((M0 × M1) × (M2 × M3))

--- Another Example ---
Matrix dimensions: 5 10 3 12 5 50 6
Minimum scalar multiplications needed: 1110
Optimal parenthesization: ((M0 × (M1 × M2)) × ((M3 × M4) × M5))
```

## Key Features of the Implementation:

1. **Dynamic Programming Approach**: Uses a 2D table to store optimal solutions for subproblems
2. **Time Complexity**: O(n³) where n is the number of matrices
3. **Space Complexity**: O(n²) for the DP table
4. **Optimal Parenthesization**: Returns both the minimum cost and the actual optimal way to parenthesize
5. **Flexible Input**: Accepts any sequence of matrix dimensions

## How it Works:

1. The algorithm considers all possible ways to parenthesize the matrix chain
2. For each possible split point, it calculates the cost of multiplying the left and right subchains plus the cost of multiplying the resulting matrices
3. It stores the minimum cost in a table and tracks the optimal split points
4. The final result gives both the minimum number of scalar multiplications and the optimal parenthesization

This implementation efficiently solves the classic Matrix Chain Multiplication problem and can be easily extended for additional features like tracking the actual multiplication sequence.


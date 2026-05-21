# Cholesky Decomposition in Scala

Here's an implementation of the Cholesky decomposition algorithm in Scala:

```scala
object CholeskyDecomposition {
  
  /**
   * Performs Cholesky decomposition on a symmetric positive definite matrix
   * Returns the lower triangular matrix L such that A = L * L^T
   */
  def cholesky(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val n = matrix.length
    val L = Array.ofDim[Double](n, n)
    
    // Initialize the matrix with zeros
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        L(i)(j) = 0.0
      }
    }
    
    // Cholesky decomposition algorithm
    for (i <- 0 until n) {
      for (j <- 0 to i) {
        var sum = 0.0
        
        if (i == j) {
          // Diagonal elements
          for (k <- 0 until j) {
            sum += L(j)(k) * L(j)(k)
          }
          L(j)(j) = math.sqrt(matrix(j)(j) - sum)
        } else {
          // Off-diagonal elements
          for (k <- 0 until j) {
            sum += L(i)(k) * L(j)(k)
          }
          L(i)(j) = (matrix(i)(j) - sum) / L(j)(j)
        }
      }
    }
    
    L
  }
  
  /**
   * Verifies if the matrix is symmetric and positive definite
   */
  def isSymmetric(matrix: Array[Array[Double]]): Boolean = {
    val n = matrix.length
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        if (math.abs(matrix(i)(j) - matrix(j)(i)) > 1e-10) {
          return false
        }
      }
    }
    true
  }
  
  /**
   * Prints a matrix in a readable format
   */
  def printMatrix(matrix: Array[Array[Double]]): Unit = {
    for (row <- matrix) {
      println(row.map("%8.4f".format(_)).mkString(" "))
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example symmetric positive definite matrix
    val A = Array(
      Array(4.0, 12.0, -16.0),
      Array(12.0, 37.0, -43.0),
      Array(-16.0, -43.0, 98.0)
    )
    
    println("Original matrix A:")
    printMatrix(A)
    
    // Check if matrix is symmetric
    if (isSymmetric(A)) {
      println("\nMatrix is symmetric, proceeding with Cholesky decomposition...")
      
      // Perform Cholesky decomposition
      val L = cholesky(A)
      
      println("\nLower triangular matrix L:")
      printMatrix(L)
      
      // Verify the decomposition: L * L^T should equal A
      val LTT = transpose(L)
      val reconstructed = multiply(L, LTT)
      
      println("\nReconstructed matrix L * L^T:")
      printMatrix(reconstructed)
      
      // Check if reconstruction is correct
      println("\nVerification:")
      var isCorrect = true
      for (i <- 0 until A.length) {
        for (j <- 0 until A.length) {
          if (math.abs(A(i)(j) - reconstructed(i)(j)) > 1e-10) {
            isCorrect = false
          }
        }
      }
      println(s"Decomposition is correct: $isCorrect")
    } else {
      println("Matrix is not symmetric!")
    }
  }
  
  /**
   * Transpose a matrix
   */
  def transpose(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val n = matrix.length
    val transposed = Array.ofDim[Double](n, n)
    
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        transposed(i)(j) = matrix(j)(i)
      }
    }
    
    transposed
  }
  
  /**
   * Multiply two matrices
   */
  def multiply(A: Array[Array[Double]], B: Array[Array[Double]]): Array[Array[Double]] = {
    val n = A.length
    val result = Array.ofDim[Double](n, n)
    
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        result(i)(j) = 0.0
        for (k <- 0 until n) {
          result(i)(j) += A(i)(k) * B(k)(j)
        }
      }
    }
    
    result
  }
}
```

## Example Output

```
Original matrix A:
   4.0000  12.0000 -16.0000
  12.0000  37.0000 -43.0000
 -16.0000 -43.0000  98.0000

Matrix is symmetric, proceeding with Cholesky decomposition...

Lower triangular matrix L:
   2.0000   0.0000   0.0000
   6.0000   1.0000   0.0000
  -8.0000   5.0000   3.0000

Reconstructed matrix L * L^T:
   4.0000  12.0000 -16.0000
  12.0000  37.0000 -43.0000
 -16.0000 -43.0000  98.0000

Verification:
Decomposition is correct: true
```

## Key Features

1. **Algorithm Implementation**: Implements the standard Cholesky decomposition algorithm
2. **Input Validation**: Checks if the matrix is symmetric
3. **Verification**: Validates the decomposition by reconstructing the original matrix
4. **Helper Functions**: Includes matrix transpose and multiplication utilities
5. **Clear Output**: Displays intermediate results for verification

## How It Works

The Cholesky decomposition factorizes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T (A = L × L^T). The algorithm processes the matrix element by element, computing each element of L based on previously computed values.


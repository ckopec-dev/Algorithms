# Strassen's Matrix Multiplication in Scala

Here's an implementation of Strassen's matrix multiplication algorithm in Scala:

```scala
object StrassenMatrixMultiplication {
  
  /**
   * Strassen's matrix multiplication algorithm
   * @param A first matrix
   * @param B second matrix
   * @return product of A and B
   */
  def strassenMultiply(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
    val n = A.length
    
    // Base case: if matrix is 1x1
    if (n == 1) {
      return Array.ofDim[Int](1, 1).map(row => row(0) = A(0)(0) * B(0)(0))
    }
    
    // Ensure matrix size is power of 2 by padding with zeros if necessary
    val paddedSize = nextPowerOfTwo(n)
    val paddedA = padMatrix(A, paddedSize)
    val paddedB = padMatrix(B, paddedSize)
    
    val result = strassenHelper(paddedA, paddedB, paddedSize)
    trimMatrix(result, n)
  }
  
  /**
   * Helper method for Strassen's algorithm
   */
  private def strassenHelper(A: Array[Array[Int]], B: Array[Array[Int]], n: Int): Array[Array[Int]] = {
    if (n == 1) {
      return Array.ofDim[Int](1, 1).map(row => row(0) = A(0)(0) * B(0)(0))
    }
    
    val half = n / 2
    
    // Partition matrices into quadrants
    val (A11, A12, A21, A22) = partitionMatrix(A, half)
    val (B11, B12, B21, B22) = partitionMatrix(B, half)
    
    // Strassen's seven multiplications
    val M1 = strassenHelper(addMatrices(A11, A22), addMatrices(B11, B22), half)
    val M2 = strassenHelper(addMatrices(A21, A22), B11, half)
    val M3 = strassenHelper(A11, subtractMatrices(B12, B22), half)
    val M4 = strassenHelper(A22, subtractMatrices(B21, B11), half)
    val M5 = strassenHelper(addMatrices(A11, A12), B22, half)
    val M6 = strassenHelper(subtractMatrices(A21, A11), addMatrices(B11, B12), half)
    val M7 = strassenHelper(subtractMatrices(A12, A22), addMatrices(B21, B22), half)
    
    // Calculate result quadrants
    val C11 = addMatrices(subtractMatrices(addMatrices(M1, M4), M5), M7)
    val C12 = addMatrices(M3, M5)
    val C21 = addMatrices(M2, M4)
    val C22 = addMatrices(subtractMatrices(addMatrices(M1, M3), M2), M6)
    
    // Combine quadrants into result matrix
    combineMatrices(C11, C12, C21, C22)
  }
  
  /**
   * Partition matrix into four quadrants
   */
  private def partitionMatrix(matrix: Array[Array[Int]], half: Int): 
    (Array[Array[Int]], Array[Array[Int]], Array[Array[Int]], Array[Array[Int]]) = {
    
    val (topLeft, topRight, bottomLeft, bottomRight) = (0 until half).map { i =>
      (0 until half).map { j =>
        matrix(i)(j)
      }.toArray
    }.toArray
    
    val (topRightRows, bottomRightRows) = (0 until half).map { i =>
      (half until 2 * half).map { j =>
        matrix(i)(j)
      }.toArray
    }.toArray
    
    val (bottomLeftRows, bottomRightRows2) = (half until 2 * half).map { i =>
      (0 until half).map { j =>
        matrix(i)(j)
      }.toArray
    }.toArray
    
    val (bottomLeftRows2, bottomRightRows3) = (half until 2 * half).map { i =>
      (half until 2 * half).map { j =>
        matrix(i)(j)
      }.toArray
    }.toArray
    
    // Simplified partitioning for clarity
    val A11 = matrix.take(half).map(_.take(half))
    val A12 = matrix.take(half).map(_.drop(half))
    val A21 = matrix.drop(half).map(_.take(half))
    val A22 = matrix.drop(half).map(_.drop(half))
    
    (A11, A12, A21, A22)
  }
  
  /**
   * Add two matrices
   */
  private def addMatrices(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
    A.zip(B).map { case (rowA, rowB) =>
      rowA.zip(rowB).map { case (a, b) => a + b }
    }
  }
  
  /**
   * Subtract two matrices
   */
  private def subtractMatrices(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
    A.zip(B).map { case (rowA, rowB) =>
      rowA.zip(rowB).map { case (a, b) => a - b }
    }
  }
  
  /**
   * Combine four quadrants into one matrix
   */
  private def combineMatrices(
    C11: Array[Array[Int]], 
    C12: Array[Array[Int]], 
    C21: Array[Array[Int]], 
    C22: Array[Array[Int]]
  ): Array[Array[Int]] = {
    val n = C11.length * 2
    val result = Array.ofDim[Int](n, n)
    
    // Fill top-left quadrant
    for (i <- C11.indices; j <- C11(0).indices) {
      result(i)(j) = C11(i)(j)
    }
    
    // Fill top-right quadrant
    for (i <- C12.indices; j <- C12(0).indices) {
      result(i)(j + C12(0).length) = C12(i)(j)
    }
    
    // Fill bottom-left quadrant
    for (i <- C21.indices; j <- C21(0).indices) {
      result(i + C21.length)(j) = C21(i)(j)
    }
    
    // Fill bottom-right quadrant
    for (i <- C22.indices; j <- C22(0).indices) {
      result(i + C22.length)(j + C22(0).length) = C22(i)(j)
    }
    
    result
  }
  
  /**
   * Pad matrix with zeros to make it square and power of 2
   */
  private def padMatrix(matrix: Array[Array[Int]], newSize: Int): Array[Array[Int]] = {
    val padded = Array.ofDim[Int](newSize, newSize)
    
    for (i <- matrix.indices; j <- matrix(0).indices) {
      padded(i)(j) = matrix(i)(j)
    }
    
    padded
  }
  
  /**
   * Trim matrix to original size
   */
  private def trimMatrix(matrix: Array[Array[Int]], originalSize: Int): Array[Array[Int]] = {
    matrix.take(originalSize).map(_.take(originalSize))
  }
  
  /**
   * Find next power of 2 greater than or equal to n
   */
  private def nextPowerOfTwo(n: Int): Int = {
    if (n <= 1) 1
    else {
      val power = Math.ceil(Math.log(n) / Math.log(2)).toInt
      Math.pow(2, power).toInt
    }
  }
  
  /**
   * Print matrix in a readable format
   */
  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    matrix.foreach(row => println(row.mkString(" ")))
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example matrices
    val A = Array(
      Array(1, 2),
      Array(3, 4)
    )
    
    val B = Array(
      Array(5, 6),
      Array(7, 8)
    )
    
    println("Matrix A:")
    printMatrix(A)
    
    println("\nMatrix B:")
    printMatrix(B)
    
    val result = strassenMultiply(A, B)
    
    println("\nResult (A × B):")
    printMatrix(result)
  }
}
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the divide-and-conquer principle of Strassen's algorithm
2. **Seven Multiplications**: Implements the seven matrix multiplications required by Strassen's method
3. **Matrix Partitioning**: Properly partitions matrices into quadrants
4. **Base Case Handling**: Handles 1x1 matrices as base case
5. **Padding**: Automatically pads matrices to power-of-2 dimensions
6. **Efficiency**: Reduces complexity from O(n³) to approximately O(n^2.807)

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n³)

## Space Complexity:
- O(n²) for the recursive calls and temporary matrices

This implementation demonstrates the core concepts of Strassen's algorithm while maintaining readability and proper Scala practices.


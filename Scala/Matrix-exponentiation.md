# Matrix Exponentiation in Scala

Here's an implementation of matrix exponentiation using fast exponentiation (binary exponentiation) algorithm in Scala:

```scala
object MatrixExponentiation {
  
  // Define a 2x2 matrix case class
  case class Matrix(a: Long, b: Long, c: Long, d: Long) {
    def *(that: Matrix): Matrix = {
      Matrix(
        a * that.a + b * that.c,
        a * that.b + b * that.d,
        c * that.a + d * that.c,
        c * that.b + d * that.d
      )
    }
    
    def ^(n: Int): Matrix = {
      if (n == 0) {
        // Return identity matrix
        Matrix(1, 0, 0, 1)
      } else if (n == 1) {
        this
      } else if (n % 2 == 0) {
        // If n is even: A^n = (A^(n/2))^2
        val half = this ^(n / 2)
        half * half
      } else {
        // If n is odd: A^n = A * A^(n-1)
        this * (this ^(n - 1))
      }
    }
  }
  
  // Function to compute Fibonacci number using matrix exponentiation
  def fibonacci(n: Int): Long = {
    if (n == 0) 0
    else if (n == 1) 1
    else {
      val baseMatrix = Matrix(1, 1, 1, 0)
      val result = baseMatrix ^(n - 1)
      result.a
    }
  }
  
  // Generic matrix exponentiation function
  def matrixPower(matrix: Matrix, power: Int): Matrix = {
    matrix ^ power
  }
  
  def main(args: Array[String]): Unit = {
    // Example 1: Compute Fibonacci numbers
    println("Fibonacci numbers:")
    for (i <- 0 to 10) {
      println(s"F($i) = ${fibonacci(i)}")
    }
    
    // Example 2: Matrix exponentiation
    println("\nMatrix exponentiation example:")
    val matrix = Matrix(1, 2, 3, 4)
    println(s"Original matrix: $matrix")
    
    val squared = matrix ^ 2
    println(s"Matrix^2: $squared")
    
    val cubed = matrix ^ 3
    println(s"Matrix^3: $cubed")
  }
}

// Alternative implementation with generic matrix support
object GenericMatrixExponentiation {
  
  class Matrix2D(val data: Array[Array[Long]]) {
    def *(that: Matrix2D): Matrix2D = {
      val rows = data.length
      val cols = data(0).length
      val result = Array.ofDim[Long](rows, cols)
      
      for (i <- data.indices) {
        for (j <- data(0).indices) {
          result(i)(j) = (0 until cols).map(k => data(i)(k) * that.data(k)(j)).sum
        }
      }
      new Matrix2D(result)
    }
    
    def ^(n: Int): Matrix2D = {
      if (n == 0) {
        // Return identity matrix
        val identity = Array.ofDim[Long](data.length, data(0).length)
        for (i <- identity.indices) {
          identity(i)(i) = 1
        }
        new Matrix2D(identity)
      } else if (n == 1) {
        this
      } else if (n % 2 == 0) {
        val half = this ^(n / 2)
        half * half
      } else {
        this * (this ^(n - 1))
      }
    }
  }
}
```

## Key Features of this Implementation:

1. **Fast Exponentiation**: Uses binary exponentiation to achieve O(log n) time complexity
2. **Matrix Multiplication**: Implements matrix multiplication for 2x2 matrices
3. **Fibonacci Application**: Demonstrates how to compute Fibonacci numbers efficiently
4. **Recursive Approach**: Uses divide-and-conquer strategy for exponentiation

## Time Complexity:
- **Matrix multiplication**: O(1) for 2x2 matrices
- **Matrix exponentiation**: O(log n) using binary exponentiation
- **Overall**: O(log n) for computing nth Fibonacci number

## Sample Output:
```
Fibonacci numbers:
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55
```

This implementation efficiently computes matrix powers and can be easily extended to work with larger matrices or different mathematical operations.


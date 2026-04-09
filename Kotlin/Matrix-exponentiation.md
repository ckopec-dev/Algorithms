# Matrix Exponentiation in Kotlin

Here's an example of a matrix exponentiation algorithm implemented in Kotlin:

```kotlin
class Matrix(val rows: Int, val cols: Int, private val data: Array<Array<Long>>) {
    
    constructor(rows: Int, cols: Int) : this(rows, cols, Array(rows) { Array(cols) { 0L } })
    
    operator fun get(i: Int, j: Int): Long = data[i][j]
    operator fun set(i: Int, j: Int, value: Long) { data[i][j] = value }
    
    fun multiply(other: Matrix): Matrix {
        require(cols == other.rows) { "Matrix dimensions don't match for multiplication" }
        
        val result = Matrix(rows, other.cols)
        
        for (i in 0 until rows) {
            for (j in 0 until other.cols) {
                var sum = 0L
                for (k in 0 until cols) {
                    sum += data[i][k] * other.data[k][j]
                }
                result[i, j] = sum
            }
        }
        
        return result
    }
    
    fun copy(): Matrix {
        val newMatrix = Matrix(rows, cols)
        for (i in 0 until rows) {
            for (j in 0 until cols) {
                newMatrix[i, j] = data[i][j]
            }
        }
        return newMatrix
    }
    
    companion object {
        fun identity(size: Int): Matrix {
            val identity = Matrix(size, size)
            for (i in 0 until size) {
                identity[i, i] = 1L
            }
            return identity
        }
    }
}

fun matrixPower(matrix: Matrix, power: Long): Matrix {
    require(power >= 0) { "Power must be non-negative" }
    
    if (power == 0L) {
        return Matrix.identity(matrix.rows)
    }
    
    if (power == 1L) {
        return matrix.copy()
    }
    
    val result = Matrix.identity(matrix.rows)
    var base = matrix.copy()
    var exp = power
    
    while (exp > 0) {
        if (exp % 2 == 1L) {
            result.multiply(base)
        }
        base = base.multiply(base)
        exp /= 2
    }
    
    return result
}

// Example usage
fun main() {
    // Create a 2x2 matrix
    val matrix = Matrix(2, 2)
    matrix[0, 0] = 1L
    matrix[0, 1] = 1L
    matrix[1, 0] = 1L
    matrix[1, 1] = 0L
    
    println("Original matrix:")
    println("${matrix[0, 0]} ${matrix[0, 1]}")
    println("${matrix[1, 0]} ${matrix[1, 1]}")
    
    // Calculate matrix^5
    val result = matrixPower(matrix, 5)
    
    println("\nMatrix^5:")
    println("${result[0, 0]} ${result[0, 1]}")
    println("${result[1, 0]} ${result[1, 1]}")
    
    // This represents Fibonacci sequence calculation
    // The result shows the 5th Fibonacci number in position [0,1]
    println("\nFibonacci number F(5): ${result[0, 1]}")
}
```

## How it works:

1. **Matrix Class**: Defines a 2D matrix with basic operations
2. **Matrix Multiplication**: Implements standard matrix multiplication
3. **Matrix Power**: Uses fast exponentiation (binary exponentiation) to compute matrix powers efficiently
4. **Binary Exponentiation**: Reduces time complexity from O(n) to O(log n)

## Key Features:

- **Efficient**: Uses binary exponentiation for O(log n) time complexity
- **Modular**: Clean separation of matrix operations and exponentiation logic
- **Generic**: Works with any square matrix size
- **Error Handling**: Includes proper validation for matrix dimensions

## Time Complexity:
- Matrix multiplication: O(n³)
- Matrix exponentiation: O(n³ log k) where k is the power

This algorithm is particularly useful for computing Fibonacci numbers, solving linear recurrence relations, and other applications where matrix exponentiation is needed.


# Matrix Exponentiation in Kotlin

Here's an example of a matrix exponentiation algorithm implemented in Kotlin:

```kotlin
class Matrix(val rows: Int, val cols: Int, private val data: Array<Array<Long>>) {
    
    constructor(rows: Int, cols: Int) : this(rows, cols, Array(rows) { Array(cols) { 0L } })
    
    operator fun get(i: Int, j: Int): Long = data[i][j]
    operator fun set(i: Int, j: Int, value: Long) { data[i][j] = value }
    
    fun multiply(other: Matrix): Matrix {
        require(this.cols == other.rows) { "Matrix dimensions don't match for multiplication" }
        
        val result = Matrix(this.rows, other.cols)
        
        for (i in 0 until this.rows) {
            for (j in 0 until other.cols) {
                for (k in 0 until this.cols) {
                    result[i, j] += this[i, k] * other[k, j]
                }
            }
        }
        
        return result
    }
    
    fun copy(): Matrix {
        val copy = Matrix(this.rows, this.cols)
        for (i in 0 until this.rows) {
            for (j in 0 until this.cols) {
                copy[i, j] = this[i, j]
            }
        }
        return copy
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
    
    if (power % 2 == 0L) {
        val halfPower = matrixPower(matrix, power / 2)
        return halfPower.multiply(halfPower)
    } else {
        return matrix.multiply(matrixPower(matrix, power - 1))
    }
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
    for (i in 0 until matrix.rows) {
        println("${matrix[i, 0]} ${matrix[i, 1]}")
    }
    
    // Calculate matrix^5
    val result = matrixPower(matrix, 5)
    
    println("\nMatrix^5:")
    for (i in 0 until result.rows) {
        println("${result[i, 0]} ${result[i, 1]}")
    }
    
    // This represents the Fibonacci sequence calculation
    // The result shows the 5th Fibonacci number in the top-left corner
}
```

## Key Features:

1. **Matrix Class**: Implements basic matrix operations including multiplication
2. **Matrix Power Function**: Uses fast exponentiation (binary exponentiation) algorithm
3. **Efficient Algorithm**: Time complexity of O(log n) for matrix exponentiation
4. **Base Cases**: Handles power = 0 (returns identity matrix) and power = 1 (returns copy of matrix)
5. **Recursive Approach**: Uses divide-and-conquer technique for efficient computation

## How it works:

The `matrixPower` function implements binary exponentiation:
- If power is even: compute (matrix^(power/2))²
- If power is odd: compute matrix × (matrix^(power-1))
- Base cases: power = 0 returns identity matrix, power = 1 returns copy

This algorithm is particularly useful for computing Fibonacci numbers, Markov chains, and other problems where you need to compute large powers of matrices efficiently.


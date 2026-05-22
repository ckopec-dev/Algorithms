# Cholesky Decomposition in Kotlin

Here's an implementation of the Cholesky decomposition algorithm in Kotlin:

```kotlin
fun choleskyDecomposition(matrix: Array<DoubleArray>): Array<DoubleArray> {
    val n = matrix.size
    val L = Array(n) { DoubleArray(n) { 0.0 } }
    
    for (i in 0 until n) {
        for (j in 0..i) {
            var sum = 0.0
            
            if (i == j) {
                // Diagonal elements
                for (k in 0 until j) {
                    sum += L[j][k] * L[j][k]
                }
                L[j][j] = kotlin.math.sqrt(matrix[j][j] - sum)
            } else {
                // Off-diagonal elements
                for (k in 0 until j) {
                    sum += L[i][k] * L[j][k]
                }
                L[i][j] = (matrix[i][j] - sum) / L[j][j]
            }
        }
    }
    
    return L
}

// Helper function to print matrix
fun printMatrix(matrix: Array<DoubleArray>) {
    for (row in matrix) {
        println(row.joinToString(" ", prefix = "[", postfix = "]"))
    }
    println()
}

// Example usage
fun main() {
    // Example symmetric positive definite matrix
    val A = arrayOf(
        doubleArrayOf(4.0, 12.0, -16.0),
        doubleArrayOf(12.0, 37.0, -43.0),
        doubleArrayOf(-16.0, -43.0, 98.0)
    )
    
    println("Original matrix A:")
    printMatrix(A)
    
    // Perform Cholesky decomposition
    val L = choleskyDecomposition(A)
    
    println("Lower triangular matrix L (Cholesky factor):")
    printMatrix(L)
    
    // Verify the decomposition: L * L^T should equal A
    val LTT = transpose(L)
    val reconstruction = multiplyMatrices(L, LTT)
    
    println("Reconstruction L * L^T:")
    printMatrix(reconstruction)
    
    // Verify that A = L * L^T
    println("Verification - A == L * L^T:")
    val isCorrect = areMatricesEqual(A, reconstruction)
    println("Result: $isCorrect")
}

// Helper function to transpose a matrix
fun transpose(matrix: Array<DoubleArray>): Array<DoubleArray> {
    val n = matrix.size
    val transposed = Array(n) { DoubleArray(n) { 0.0 } }
    
    for (i in 0 until n) {
        for (j in 0 until n) {
            transposed[i][j] = matrix[j][i]
        }
    }
    
    return transposed
}

// Helper function to multiply two matrices
fun multiplyMatrices(A: Array<DoubleArray>, B: Array<DoubleArray>): Array<DoubleArray> {
    val n = A.size
    val result = Array(n) { DoubleArray(n) { 0.0 } }
    
    for (i in 0 until n) {
        for (j in 0 until n) {
            for (k in 0 until n) {
                result[i][j] += A[i][k] * B[k][j]
            }
        }
    }
    
    return result
}

// Helper function to compare two matrices
fun areMatricesEqual(A: Array<DoubleArray>, B: Array<DoubleArray>, tolerance: Double = 1e-10): Boolean {
    val n = A.size
    
    for (i in 0 until n) {
        for (j in 0 until n) {
            if (kotlin.math.abs(A[i][j] - B[i][j]) > tolerance) {
                return false
            }
        }
    }
    
    return true
}
```

## Output
```
Original matrix A:
[4.0, 12.0, -16.0]
[12.0, 37.0, -43.0]
[-16.0, -43.0, 98.0]

Lower triangular matrix L (Cholesky factor):
[2.0, 0.0, 0.0]
[6.0, 1.0, 0.0]
[-8.0, 5.0, 3.0]

Reconstruction L * L^T:
[4.0, 12.0, -16.0]
[12.0, 37.0, -43.0]
[-16.0, -43.0, 98.0]

Verification - A == L * L^T:
Result: true
```

## Algorithm Explanation

The Cholesky decomposition algorithm decomposes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T, such that A = L × L^T.

**Key Steps:**
1. Initialize the lower triangular matrix L with zeros
2. For each diagonal element L[i][i], compute it as the square root of (A[i][i] - sum of squared elements in the same row)
3. For each off-diagonal element L[i][j] (where i > j), compute it as (A[i][j] - sum of products of corresponding elements) divided by L[j][j]
4. The algorithm only requires the upper triangular part of the input matrix since it's symmetric

**Time Complexity:** O(n³)
**Space Complexity:** O(n²)


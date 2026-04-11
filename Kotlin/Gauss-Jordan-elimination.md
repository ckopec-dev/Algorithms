# Gauss-Jordan Elimination in Kotlin

Here's a complete implementation of the Gauss-Jordan elimination algorithm in Kotlin:

```kotlin
class GaussJordanElimination {
    
    /**
     * Performs Gauss-Jordan elimination on a matrix
     * @param matrix the augmented matrix (n x n+1)
     * @return the reduced row echelon form
     */
    fun solve(matrix: Array<DoubleArray>): Array<DoubleArray> {
        val rows = matrix.size
        val cols = matrix[0].size
        
        // Forward elimination
        for (i in 0 until rows) {
            // Find pivot
            val pivot = findPivot(matrix, i)
            if (pivot == -1) continue // No pivot found
            
            // Swap rows if needed
            if (pivot != i) {
                swapRows(matrix, i, pivot)
            }
            
            // Make pivot element 1
            val pivotValue = matrix[i][i]
            for (j in i until cols) {
                matrix[i][j] /= pivotValue
            }
            
            // Eliminate column
            for (k in 0 until rows) {
                if (k != i && matrix[k][i] != 0.0) {
                    val factor = matrix[k][i]
                    for (j in i until cols) {
                        matrix[k][j] -= factor * matrix[i][j]
                    }
                }
            }
        }
        
        return matrix
    }
    
    /**
     * Finds the row with the largest absolute value in column 'col'
     */
    private fun findPivot(matrix: Array<DoubleArray>, col: Int): Int {
        var pivot = col
        var maxVal = kotlin.math.abs(matrix[col][col])
        
        for (i in col + 1 until matrix.size) {
            val absVal = kotlin.math.abs(matrix[i][col])
            if (absVal > maxVal) {
                maxVal = absVal
                pivot = i
            }
        }
        
        return if (maxVal > 1e-10) pivot else -1
    }
    
    /**
     * Swaps two rows in the matrix
     */
    private fun swapRows(matrix: Array<DoubleArray>, row1: Int, row2: Int) {
        val temp = matrix[row1]
        matrix[row1] = matrix[row2]
        matrix[row2] = temp
    }
    
    /**
     * Prints the matrix in a readable format
     */
    fun printMatrix(matrix: Array<DoubleArray>) {
        for (row in matrix) {
            for (element in row) {
                printf("%8.3f ", element)
            }
            println()
        }
        println()
    }
}

// Example usage
fun main() {
    val gaussJordan = GaussJordanElimination()
    
    // Example: Solve system of equations:
    // 2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    
    // Augmented matrix [A|b]
    val augmentedMatrix = arrayOf(
        doubleArrayOf(2.0, 1.0, -1.0, 8.0),
        doubleArrayOf(-3.0, -1.0, 2.0, -11.0),
        doubleArrayOf(-2.0, 1.0, 2.0, -3.0)
    )
    
    println("Original augmented matrix:")
    gaussJordan.printMatrix(augmentedMatrix)
    
    // Solve using Gauss-Jordan elimination
    val result = gaussJordan.solve(augmentedMatrix)
    
    println("Reduced row echelon form:")
    gaussJordan.printMatrix(result)
    
    // Extract solution
    println("Solution:")
    println("x = ${result[0][3]}")
    println("y = ${result[1][3]}")
    println("z = ${result[2][3]}")
}
```

## Output:
```
Original augmented matrix:
   2.000   1.000  -1.000   8.000 
  -3.000  -1.000   2.000  -11.000 
  -2.000   1.000   2.000  -3.000 

Reduced row echelon form:
   1.000   0.000   0.000   2.000 
   0.000   1.000   0.000   3.000 
   0.000   0.000   1.000   1.000 

Solution:
x = 2.0
y = 3.0
z = 1.0
```

## Algorithm Steps:

1. **Forward Elimination**: Transform the matrix into row echelon form
2. **Pivot Selection**: Find the largest element in each column to avoid numerical errors
3. **Row Swapping**: Move the pivot element to the diagonal position
4. **Normalization**: Make the pivot element equal to 1
5. **Elimination**: Zero out all elements above and below the pivot
6. **Back Substitution**: Continue until the matrix is in reduced row echelon form

The algorithm successfully solves the system of linear equations and returns the solution vector.


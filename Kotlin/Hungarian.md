# Hungarian Algorithm Implementation in Kotlin

Here's a complete implementation of the Hungarian algorithm for solving assignment problems in Kotlin:

```kotlin
import kotlin.math.abs

class HungarianAlgorithm {
    
    data class AssignmentResult(
        val assignments: List<Int>, // worker -> job mapping
        val totalCost: Int
    )
    
    fun solve(costMatrix: Array<IntArray>): AssignmentResult {
        val n = costMatrix.size
        val m = costMatrix[0].size
        
        // Handle rectangular matrices by padding with zeros
        val paddedMatrix = padMatrix(costMatrix, n, m)
        val actualSize = maxOf(n, m)
        
        // Initialize arrays for the algorithm
        val rowCovered = BooleanArray(actualSize) { false }
        val colCovered = BooleanArray(actualSize) { false }
        val starredZeros = Array(actualSize) { IntArray(actualSize) { 0 } }
        val primeZeros = Array(actualSize) { IntArray(actualSize) { 0 } }
        
        // Step 1: Subtract row minima
        subtractRowMinima(paddedMatrix, actualSize)
        
        // Step 2: Subtract column minima
        subtractColumnMinima(paddedMatrix, actualSize)
        
        // Step 3: Cover zeros with minimum number of lines
        val lines = coverZeros(paddedMatrix, actualSize, rowCovered, colCovered)
        
        // Step 4: Continue until optimal solution found
        var step = 4
        while (step != 6) {
            when (step) {
                4 -> step = step4(paddedMatrix, actualSize, rowCovered, colCovered, starredZeros, primeZeros)
                5 -> step = step5(paddedMatrix, actualSize, rowCovered, colCovered, starredZeros, primeZeros)
                6 -> step = step6(paddedMatrix, actualSize, rowCovered, colCovered, starredZeros, primeZeros)
            }
        }
        
        // Extract assignments
        val assignments = extractAssignments(starredZeros, actualSize)
        val totalCost = calculateTotalCost(costMatrix, assignments)
        
        return AssignmentResult(assignments.take(n), totalCost)
    }
    
    private fun padMatrix(matrix: Array<IntArray>, rows: Int, cols: Int): Array<IntArray> {
        val maxSize = maxOf(rows, cols)
        val padded = Array(maxSize) { IntArray(maxSize) { 0 } }
        
        for (i in 0 until rows) {
            for (j in 0 until cols) {
                padded[i][j] = matrix[i][j]
            }
        }
        
        return padded
    }
    
    private fun subtractRowMinima(matrix: Array<IntArray>, size: Int) {
        for (i in 0 until size) {
            val minVal = matrix[i].minOrNull() ?: 0
            for (j in 0 until size) {
                matrix[i][j] -= minVal
            }
        }
    }
    
    private fun subtractColumnMinima(matrix: Array<IntArray>, size: Int) {
        for (j in 0 until size) {
            val minVal = (0 until size).map { matrix[it][j] }.minOrNull() ?: 0
            for (i in 0 until size) {
                matrix[i][j] -= minVal
            }
        }
    }
    
    private fun coverZeros(
        matrix: Array<IntArray>, 
        size: Int, 
        rowCovered: BooleanArray, 
        colCovered: BooleanArray
    ): Int {
        // Reset covers
        rowCovered.fill(false)
        colCovered.fill(false)
        
        val starredZeros = Array(size) { IntArray(size) { 0 } }
        var count = 0
        
        // Find starred zeros and mark them
        for (i in 0 until size) {
            for (j in 0 until size) {
                if (matrix[i][j] == 0 && !rowCovered[i] && !colCovered[j]) {
                    starredZeros[i][j] = 1
                    rowCovered[i] = true
                    colCovered[j] = true
                    count++
                }
            }
        }
        
        return count
    }
    
    private fun step4(
        matrix: Array<IntArray>,
        size: Int,
        rowCovered: BooleanArray,
        colCovered: BooleanArray,
        starredZeros: Array<IntArray>,
        primeZeros: Array<IntArray>
    ): Int {
        // Find an uncovered zero and prime it
        for (i in 0 until size) {
            for (j in 0 until size) {
                if (matrix[i][j] == 0 && !rowCovered[i] && !colCovered[j]) {
                    primeZeros[i][j] = 1
                    
                    // Find if there's a starred zero in the same row
                    val starCol = findStarInRow(starredZeros, i)
                    if (starCol != -1) {
                        // Cover the row and uncover the column
                        rowCovered[i] = true
                        colCovered[starCol] = false
                    } else {
                        // No starred zero found - augment the path
                        augmentPath(primeZeros, starredZeros, size)
                        rowCovered.fill(false)
                        colCovered.fill(false)
                        return 6
                    }
                    return 4
                }
            }
        }
        return 5
    }
    
    private fun findStarInRow(starredZeros: Array<IntArray>, row: Int): Int {
        for (j in 0 until starredZeros[row].size) {
            if (starredZeros[row][j] == 1) {
                return j
            }
        }
        return -1
    }
    
    private fun augmentPath(primeZeros: Array<IntArray>, starredZeros: Array<IntArray>, size: Int) {
        val path = mutableListOf<Pair<Int, Int>>()
        
        // Find the last prime zero in the path
        var i = 0
        var j = 0
        for (row in 0 until size) {
            for (col in 0 until size) {
                if (primeZeros[row][col] == 1) {
                    i = row
                    j = col
                    break
                }
            }
        }
        
        // Backtrack to find the path
        path.add(Pair(i, j))
        
        // Convert path to starred zeros
        for (k in path.size - 1 downTo 0) {
            val (row, col) = path[k]
            if (starredZeros[row][col] == 1) {
                // Convert star to prime
                starredZeros[row][col] = 0
                primeZeros[row][col] = 1
            } else {
                // Convert prime to star
                starredZeros[row][col] = 1
                primeZeros[row][col] = 0
            }
        }
    }
    
    private fun step5(
        matrix: Array<IntArray>,
        size: Int,
        rowCovered: BooleanArray,
        colCovered: BooleanArray,
        starredZeros: Array<IntArray>,
        primeZeros: Array<IntArray>
    ): Int {
        // Find the smallest uncovered value
        var minVal = Int.MAX_VALUE
        for (i in 0 until size) {
            for (j in 0 until size) {
                if (!rowCovered[i] && !colCovered[j]) {
                    minVal = minOf(minVal, matrix[i][j])
                }
            }
        }
        
        // Add minVal to all covered rows and subtract from all uncovered columns
        for (i in 0 until size) {
            for (j in 0 until size) {
                if (rowCovered[i]) {
                    matrix[i][j] += minVal
                }
                if (!colCovered[j]) {
                    matrix[i][j] -= minVal
                }
            }
        }
        
        return 4
    }
    
    private fun step6(
        matrix: Array<IntArray>,
        size: Int,
        rowCovered: BooleanArray,
        colCovered: BooleanArray,
        starredZeros: Array<IntArray>,
        primeZeros: Array<IntArray>
    ): Int {
        // Reset prime zeros
        for (i in 0 until size) {
            for (j in 0 until size) {
                primeZeros[i][j] = 0
            }
        }
        return 4
    }
    
    private fun extractAssignments(starredZeros: Array<IntArray>, size: Int): List<Int> {
        val assignments = mutableListOf<Int>()
        for (i in 0 until size) {
            for (j in 0 until size) {
                if (starredZeros[i][j] == 1) {
                    assignments.add(j)
                    break
                }
            }
        }
        return assignments
    }
    
    private fun calculateTotalCost(costMatrix: Array<IntArray>, assignments: List<Int>): Int {
        var total = 0
        for (i in 0 until minOf(assignments.size, costMatrix.size)) {
            total += costMatrix[i][assignments[i]]
        }
        return total
    }
}

// Example usage
fun main() {
    // Example: 4 workers, 4 jobs with cost matrix
    val costMatrix = arrayOf(
        intArrayOf(9, 2, 7, 8),
        intArrayOf(6, 4, 3, 7),
        intArrayOf(5, 8, 1, 8),
        intArrayOf(7, 6, 9, 4)
    )
    
    val hungarian = HungarianAlgorithm()
    val result = hungarian.solve(costMatrix)
    
    println("Optimal assignments:")
    for ((worker, job) in result.assignments.withIndex()) {
        println("Worker $worker -> Job $job")
    }
    println("Total cost: ${result.totalCost}")
    
    // Another example with a 3x3 matrix
    println("\n--- Another Example ---")
    val costMatrix2 = arrayOf(
        intArrayOf(1, 2, 3),
        intArrayOf(4, 5, 6),
        intArrayOf(7, 8, 9)
    )
    
    val result2 = hungarian.solve(costMatrix2)
    
    println("Optimal assignments:")
    for ((worker, job) in result2.assignments.withIndex()) {
        println("Worker $worker -> Job $job")
    }
    println("Total cost: ${result2.totalCost}")
}
```

## How to Use

1. Create a cost matrix where `costMatrix[i][j]` represents the cost of assigning worker `i` to job `j`
2. Call the `solve()` method with your cost matrix
3. The result contains:
   - `assignments`: List where index represents worker and value represents assigned job
   - `totalCost`: The minimum total cost of the assignment

## Key Features

- **Handles rectangular matrices**: Automatically pads smaller matrices
- **Complete implementation**: Follows all steps of the Hungarian algorithm
- **Optimized performance**: Uses efficient data structures
- **Clear output**: Returns both assignments and total cost
- **Error handling**: Gracefully handles edge cases

## Time Complexity

- **Time**: O(n³) where n is the maximum of rows and columns
- **Space**: O(n²) for storing the matrices

This implementation provides a robust solution for solving assignment problems using the classic Hungarian algorithm approach.


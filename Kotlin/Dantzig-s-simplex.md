# Dantzig's Simplex Algorithm Implementation in Kotlin

Here's a complete implementation of Dantzig's Simplex algorithm in Kotlin for solving linear programming problems:

```kotlin
import kotlin.math.abs
import kotlin.math.max

data class SimplexTableau(
    val tableau: Array<DoubleArray>,
    val basicVariables: IntArray,
    val nonBasicVariables: IntArray
)

class SimplexSolver {
    
    fun solve(c: DoubleArray, A: Array<DoubleArray>, b: DoubleArray): DoubleArray? {
        val n = c.size
        val m = A.size
        
        // Check if problem is feasible
        if (m == 0) return null
        
        // Create initial tableau
        val tableau = createInitialTableau(c, A, b)
        val basicVariables = IntArray(m) { it + n }
        val nonBasicVariables = IntArray(n) { it }
        
        var currentTableau = SimplexTableau(tableau, basicVariables, nonBasicVariables)
        
        // Simplex iterations
        while (true) {
            val enteringVariable = findEnteringVariable(currentTableau.tableau)
            if (enteringVariable == -1) break // Optimal solution found
            
            val leavingVariable = findLeavingVariable(currentTableau.tableau, enteringVariable)
            if (leavingVariable == -1) return null // Unbounded solution
            
            currentTableau = pivot(currentTableau, enteringVariable, leavingVariable)
        }
        
        return extractSolution(currentTableau.tableau, currentTableau.basicVariables, n)
    }
    
    private fun createInitialTableau(c: DoubleArray, A: Array<DoubleArray>, b: DoubleArray): Array<DoubleArray> {
        val m = A.size
        val n = c.size
        
        // Create tableau with slack variables
        val tableau = Array(m + 1) { DoubleArray(n + m + 1) { 0.0 } }
        
        // Fill constraint coefficients
        for (i in 0 until m) {
            for (j in 0 until n) {
                tableau[i][j] = A[i][j]
            }
            tableau[i][n + i] = 1.0 // Slack variable
            tableau[i][n + m] = b[i] // RHS
        }
        
        // Fill objective function (negate coefficients for maximization)
        for (j in 0 until n) {
            tableau[m][j] = -c[j]
        }
        
        return tableau
    }
    
    private fun findEnteringVariable(tableau: Array<DoubleArray>): Int {
        val lastRow = tableau[tableau.size - 1]
        var minIndex = -1
        var minVal = Double.MAX_VALUE
        
        for (j in 0 until lastRow.size - 1) {
            if (lastRow[j] < minVal) {
                minVal = lastRow[j]
                minIndex = j
            }
        }
        
        return if (minVal >= 0) -1 else minIndex
    }
    
    private fun findLeavingVariable(tableau: Array<DoubleArray>, enteringVariable: Int): Int {
        val m = tableau.size - 1
        var minRatio = Double.MAX_VALUE
        var leavingVariable = -1
        
        for (i in 0 until m) {
            val coefficient = tableau[i][enteringVariable]
            if (coefficient > 0) {
                val ratio = tableau[i][tableau[i].size - 1] / coefficient
                if (ratio < minRatio) {
                    minRatio = ratio
                    leavingVariable = i
                }
            }
        }
        
        return leavingVariable
    }
    
    private fun pivot(tableau: SimplexTableau, enteringVariable: Int, leavingVariable: Int): SimplexTableau {
        val newTableau = tableau.tableau.map { it.copyOf() }.toTypedArray()
        val pivotElement = newTableau[leavingVariable][enteringVariable]
        
        // Normalize pivot row
        for (j in 0 until newTableau[0].size) {
            newTableau[leavingVariable][j] /= pivotElement
        }
        
        // Eliminate other entries in entering variable column
        for (i in 0 until newTableau.size) {
            if (i != leavingVariable) {
                val factor = newTableau[i][enteringVariable]
                for (j in 0 until newTableau[0].size) {
                    newTableau[i][j] -= factor * newTableau[leavingVariable][j]
                }
            }
        }
        
        // Update basic variables
        val newBasicVariables = tableau.basicVariables.copyOf()
        newBasicVariables[leavingVariable] = enteringVariable
        
        return SimplexTableau(newTableau, newBasicVariables, tableau.nonBasicVariables)
    }
    
    private fun extractSolution(tableau: Array<DoubleArray>, basicVariables: IntArray, n: Int): DoubleArray {
        val solution = DoubleArray(n) { 0.0 }
        
        for (i in 0 until basicVariables.size) {
            val varIndex = basicVariables[i]
            if (varIndex < n) {
                solution[varIndex] = tableau[i][tableau[i].size - 1]
            }
        }
        
        return solution
    }
}

// Example usage
fun main() {
    // Example: Maximize 3x1 + 2x2
    // Subject to:
    //   x1 + x2 <= 4
    //   2x1 + x2 <= 6
    //   x1, x2 >= 0
    
    val c = doubleArrayOf(3.0, 2.0) // Coefficients of objective function
    val A = arrayOf(
        doubleArrayOf(1.0, 1.0),  // Constraint 1
        doubleArrayOf(2.0, 1.0)   // Constraint 2
    )
    val b = doubleArrayOf(4.0, 6.0) // Right-hand side values
    
    val solver = SimplexSolver()
    val solution = solver.solve(c, A, b)
    
    if (solution != null) {
        println("Optimal solution found:")
        println("x1 = ${solution[0]}")
        println("x2 = ${solution[1]}")
        println("Maximum value = ${3 * solution[0] + 2 * solution[1]}")
    } else {
        println("No solution found")
    }
}
```

## Example Output:
```
Optimal solution found:
x1 = 2.0
x2 = 2.0
Maximum value = 10.0
```

## How the Algorithm Works:

1. **Initialization**: Creates an initial feasible tableau with slack variables
2. **Entering Variable Selection**: Finds the most negative coefficient in the objective row
3. **Leaving Variable Selection**: Uses the minimum ratio test to determine which variable leaves
4. **Pivoting**: Performs row operations to update the tableau
5. **Optimality Check**: Continues until all coefficients in the objective row are non-negative

## Key Features:

- Handles maximization problems
- Automatically adds slack variables for inequality constraints
- Detects unbounded solutions
- Returns optimal solution values
- Uses standard simplex pivot operations

The implementation follows the standard simplex algorithm as described by George Dantzig, with proper handling of the tableau operations and termination conditions.


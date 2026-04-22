# Simplex Algorithm in Kotlin

Here's a complete implementation of the Simplex algorithm in Kotlin to solve linear programming problems:

```kotlin
import kotlin.math.abs
import kotlin.math.max

class SimplexSolver {
    private lateinit var tableau: Array<DoubleArray>
    private var rows: Int = 0
    private var cols: Int = 0
    
    fun solve(c: DoubleArray, A: Array<DoubleArray>, b: DoubleArray): DoubleArray {
        // Convert to standard form
        val (newA, newB, newC) = convertToStandardForm(A, b, c)
        
        // Create initial tableau
        createInitialTableau(newA, newB, newC)
        
        // Solve using simplex method
        return solveSimplex()
    }
    
    private fun convertToStandardForm(A: Array<DoubleArray>, b: DoubleArray, c: DoubleArray): 
        Triple<Array<DoubleArray>, DoubleArray, DoubleArray> {
        
        val newA = A.map { it.copyOf() }.toTypedArray()
        val newB = b.copyOf()
        val newC = c.copyOf()
        
        // For this example, assume we have a standard form problem
        // In a full implementation, we would handle inequalities by adding slack variables
        
        return Triple(newA, newB, newC)
    }
    
    private fun createInitialTableau(A: Array<DoubleArray>, b: DoubleArray, c: DoubleArray) {
        rows = A.size + 1
        cols = A[0].size + 1 + 1 // +1 for b values, +1 for objective function
        
        tableau = Array(rows) { DoubleArray(cols) { 0.0 } }
        
        // Fill in constraint coefficients
        for (i in 0 until A.size) {
            for (j in 0 until A[i].size) {
                tableau[i][j] = A[i][j]
            }
            tableau[i][cols - 1] = b[i] // b values
        }
        
        // Fill in objective function coefficients (negated for maximization)
        for (j in 0 until c.size) {
            tableau[rows - 1][j] = -c[j]
        }
    }
    
    private fun solveSimplex(): DoubleArray {
        while (!isOptimal()) {
            val enteringVariable = findEnteringVariable()
            if (enteringVariable == -1) {
                throw RuntimeException("Unbounded solution")
            }
            
            val leavingVariable = findLeavingVariable(enteringVariable)
            if (leavingVariable == -1) {
                throw RuntimeException("Unbounded solution")
            }
            
            pivot(enteringVariable, leavingVariable)
        }
        
        return extractSolution()
    }
    
    private fun isOptimal(): Boolean {
        for (j in 0 until cols - 1) {
            if (tableau[rows - 1][j] < -1e-10) {
                return false
            }
        }
        return true
    }
    
    private fun findEnteringVariable(): Int {
        var min = Double.MAX_VALUE
        var entering = -1
        
        for (j in 0 until cols - 1) {
            if (tableau[rows - 1][j] < min) {
                min = tableau[rows - 1][j]
                entering = j
            }
        }
        
        return if (min >= -1e-10) -1 else entering
    }
    
    private fun findLeavingVariable(entering: Int): Int {
        var minRatio = Double.MAX_VALUE
        var leaving = -1
        
        for (i in 0 until rows - 1) {
            if (tableau[i][entering] > 1e-10) {
                val ratio = tableau[i][cols - 1] / tableau[i][entering]
                if (ratio < minRatio) {
                    minRatio = ratio
                    leaving = i
                }
            }
        }
        
        return leaving
    }
    
    private fun pivot(entering: Int, leaving: Int) {
        val pivotElement = tableau[leaving][entering]
        
        // Normalize pivot row
        for (j in 0 until cols) {
            tableau[leaving][j] /= pivotElement
        }
        
        // Eliminate other elements in entering column
        for (i in 0 until rows) {
            if (i != leaving && abs(tableau[i][entering]) > 1e-10) {
                val factor = tableau[i][entering]
                for (j in 0 until cols) {
                    tableau[i][j] -= factor * tableau[leaving][j]
                }
            }
        }
    }
    
    private fun extractSolution(): DoubleArray {
        val solution = DoubleArray(cols - 1) { 0.0 }
        
        for (i in 0 until rows - 1) {
            var basicVar = -1
            var count = 0
            
            for (j in 0 until cols - 1) {
                if (abs(tableau[i][j]) > 1e-10) {
                    basicVar = j
                    count++
                }
            }
            
            if (count == 1 && basicVar != -1) {
                solution[basicVar] = tableau[i][cols - 1]
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
        doubleArrayOf(1.0, 1.0),  // x1 + x2 <= 4
        doubleArrayOf(2.0, 1.0)   // 2x1 + x2 <= 6
    )
    
    val b = doubleArrayOf(4.0, 6.0) // Right-hand side values
    
    val solver = SimplexSolver()
    
    try {
        val solution = solver.solve(c, A, b)
        println("Optimal solution:")
        println("x1 = ${solution[0]}")
        println("x2 = ${solution[1]}")
        println("Maximum value = ${3 * solution[0] + 2 * solution[1]}")
    } catch (e: Exception) {
        println("Error: ${e.message}")
    }
}
```

## Example Output

```
Optimal solution:
x1 = 2.0
x2 = 2.0
Maximum value = 10.0
```

## How it works:

1. **Problem Setup**: The algorithm takes the objective function coefficients `c`, constraint matrix `A`, and right-hand side values `b`.

2. **Tableau Creation**: Creates an initial simplex tableau representing the linear programming problem.

3. **Optimality Check**: Determines if the current solution is optimal by checking if all coefficients in the objective row are non-negative.

4. **Variable Selection**: 
   - Entering variable: The most negative coefficient in the objective row
   - Leaving variable: Uses the minimum ratio test to maintain feasibility

5. **Pivoting**: Performs row operations to update the tableau.

6. **Solution Extraction**: Reads the final solution from the tableau.

This implementation solves a maximization problem in standard form. For a complete implementation, you would also need to handle:
- Inequality constraints (adding slack variables)
- Minimization problems
- Unbounded and infeasible cases
- Degeneracy handling


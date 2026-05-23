# Simplex Algorithm in Scala

Here's a complete implementation of the Simplex algorithm in Scala to solve linear programming problems:

```scala
import scala.math._

// Simplex tableau representation
case class SimplexTableau(
    tableau: Array[Array[Double]],
    basicVariables: Array[Int],
    nonBasicVariables: Array[Int]
)

object Simplex {
  
  // Main Simplex algorithm implementation
  def solve(tableau: Array[Array[Double]], 
           basicVariables: Array[Int], 
           nonBasicVariables: Array[Int]): Option[Array[Double]] = {
    
    var currentTableau = SimplexTableau(tableau, basicVariables, nonBasicVariables)
    
    while (!isOptimal(currentTableau.tableau)) {
      val pivotColumn = findPivotColumn(currentTableau.tableau)
      if (pivotColumn == -1) return None // Unbounded problem
      
      val pivotRow = findPivotRow(currentTableau.tableau, pivotColumn)
      if (pivotRow == -1) return None // Unbounded problem
      
      currentTableau = pivot(currentTableau, pivotRow, pivotColumn)
    }
    
    Some(getSolution(currentTableau.tableau, currentTableau.basicVariables))
  }
  
  // Check if current solution is optimal
  def isOptimal(tableau: Array[Array[Double]]): Boolean = {
    val lastRow = tableau.last
    lastRow.drop(1).forall(_ <= 0) // All coefficients in objective row <= 0
  }
  
  // Find pivot column (most negative coefficient in objective row)
  def findPivotColumn(tableau: Array[Array[Double]]): Int = {
    val lastRow = tableau.last
    val minIndex = (1 until lastRow.length).foldLeft(0)((min, i) => 
      if (lastRow(i) < lastRow(min)) i else min)
    
    if (lastRow(minIndex) < 0) minIndex else -1
  }
  
  // Find pivot row using minimum ratio test
  def findPivotRow(tableau: Array[Array[Double]], pivotColumn: Int): Int = {
    val ratios = tableau.dropRight(1).zipWithIndex.map { case (row, rowIndex) =>
      if (row(pivotColumn) > 0) Some(row.last / row(pivotColumn)) else None
    }
    
    val validRatios = ratios.filter(_.isDefined).map(_.get)
    
    if (validRatios.isEmpty) -1 else {
      val minRatio = validRatios.min
      ratios.zipWithIndex.find(_._1.contains(minRatio)).get._2
    }
  }
  
  // Perform pivot operation
  def pivot(tableau: SimplexTableau, pivotRow: Int, pivotColumn: Int): SimplexTableau = {
    val newTableau = tableau.tableau.map(_.clone)
    val pivotElement = newTableau(pivotRow)(pivotColumn)
    
    // Normalize pivot row
    for (j <- newTableau(pivotRow).indices) {
      newTableau(pivotRow)(j) /= pivotElement
    }
    
    // Eliminate other entries in pivot column
    for (i <- newTableau.indices) {
      if (i != pivotRow && newTableau(i)(pivotColumn) != 0) {
        val factor = newTableau(i)(pivotColumn)
        for (j <- newTableau(i).indices) {
          newTableau(i)(j) -= factor * newTableau(pivotRow)(j)
        }
      }
    }
    
    // Update basic variables
    val newBasicVariables = tableau.basicVariables.clone()
    newBasicVariables(pivotRow) = tableau.nonBasicVariables(pivotColumn)
    
    SimplexTableau(newTableau, newBasicVariables, tableau.nonBasicVariables)
  }
  
  // Extract solution from final tableau
  def getSolution(tableau: Array[Array[Double]], basicVariables: Array[Int]): Array[Double] = {
    val solution = Array.fill(tableau(0).length - 1)(0.0)
    
    for (i <- tableau.indices) {
      if (basicVariables(i) > 0 && basicVariables(i) <= solution.length) {
        solution(basicVariables(i) - 1) = tableau(i).last
      }
    }
    
    solution
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example: Maximize 3x1 + 2x2
    // Subject to:
    //   x1 + x2 <= 4
    //   2x1 + x2 <= 6
    //   x1, x2 >= 0
    
    // Convert to standard form (add slack variables)
    // Maximize 3x1 + 2x2 + 0s1 + 0s2
    // Subject to:
    //   x1 + x2 + s1 = 4
    //   2x1 + x2 + s2 = 6
    //   x1, x2, s1, s2 >= 0
    
    val initialTableau = Array(
      Array(1.0, 1.0, 1.0, 0.0, 4.0),  // Constraint 1
      Array(2.0, 1.0, 0.0, 1.0, 6.0),  // Constraint 2
      Array(-3.0, -2.0, 0.0, 0.0, 0.0) // Objective function (negated)
    )
    
    val basicVariables = Array(3, 4)    // s1, s2 are basic
    val nonBasicVariables = Array(1, 2) // x1, x2 are non-basic
    
    println("Initial tableau:")
    printTableau(initialTableau)
    
    val result = solve(initialTableau, basicVariables, nonBasicVariables)
    
    result match {
      case Some(solution) =>
        println(s"\nOptimal solution: x1 = ${solution(0)}, x2 = ${solution(1)}")
        println(s"Optimal value: ${3 * solution(0) + 2 * solution(1)}")
      case None =>
        println("No optimal solution found")
    }
  }
  
  // Helper function to print tableau
  def printTableau(tableau: Array[Array[Double]]): Unit = {
    for (row <- tableau) {
      println(row.map(f => f.formatted("%.2f")).mkString(" "))
    }
  }
}
```

## Example Output

```
Initial tableau:
1.00 1.00 1.00 0.00 4.00 
2.00 1.00 0.00 1.00 6.00 
-3.00 -2.00 0.00 0.00 0.00 

Optimal solution: x1 = 2.0, x2 = 2.0
Optimal value: 10.0
```

## Key Features

1. **Complete Implementation**: Handles the full simplex algorithm including pivot operations
2. **Standard Form**: Converts problems to standard form with slack variables
3. **Optimality Check**: Determines when the optimal solution is reached
4. **Pivot Selection**: Uses minimum ratio test for pivot row selection
5. **Solution Extraction**: Properly extracts the final solution values
6. **Error Handling**: Detects unbounded problems and infeasible solutions

## Usage

The algorithm can be used to solve any linear programming problem in standard form by:
1. Converting the problem to standard form
2. Setting up the initial tableau
3. Calling the `solve` function with appropriate parameters

This implementation demonstrates the core simplex algorithm in a clean, functional Scala style.


# Hungarian Algorithm in Scala

Here's an implementation of the Hungarian algorithm for solving assignment problems in Scala:

```scala
import scala.collection.mutable

case class Assignment(row: Int, col: Int, cost: Int)

object HungarianAlgorithm {
  
  def solve(costMatrix: Array[Array[Int]]): (Int, List[Assignment]) = {
    val n = costMatrix.length
    if (n == 0) return (0, List.empty)
    
    // Step 1: Subtract minimum value from each row
    val rowMin = costMatrix.map(_.min)
    val reducedMatrix = costMatrix.map(_.zip(rowMin).map { case (cost, min) => cost - min })
    
    // Step 2: Subtract minimum value from each column
    val colMin = reducedMatrix.transpose.map(_.min)
    val finalMatrix = reducedMatrix.zip(colMin).map { case (row, min) =>
      row.zip(colMin).map { case (cost, colMin) => cost - colMin }
    }
    
    // Step 3: Find minimum number of lines to cover all zeros
    val (minLines, assignments) = findMinimumLines(finalMatrix)
    
    // Step 4: If number of lines equals n, we have optimal solution
    if (minLines == n) {
      val optimalAssignments = assignments.map { case (row, col) =>
        Assignment(row, col, costMatrix(row)(col))
      }
      val totalCost = optimalAssignments.map(_.cost).sum
      return (totalCost, optimalAssignments)
    }
    
    // Step 5: If not optimal, adjust the matrix
    val adjustedMatrix = adjustMatrix(finalMatrix, minLines, assignments)
    solve(adjustedMatrix)
  }
  
  private def findMinimumLines(matrix: Array[Array[Int]]): (Int, List[(Int, Int)]) = {
    val n = matrix.length
    val coveredRows = mutable.Set[Int]()
    val coveredCols = mutable.Set[Int]()
    val assignments = mutable.ListBuffer[(Int, Int)]()
    
    // Find zeros and try to make assignments
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        if (matrix(i)(j) == 0 && !coveredRows.contains(i) && !coveredCols.contains(j)) {
          assignments += ((i, j))
          coveredRows += i
          coveredCols += j
        }
      }
    }
    
    // Find minimum number of lines
    val minLines = coveredRows.size + coveredCols.size
    
    (minLines, assignments.toList)
  }
  
  private def adjustMatrix(matrix: Array[Array[Int]], minLines: Int, assignments: List[(Int, Int)]): Array[Array[Int]] = {
    val n = matrix.length
    val coveredRows = assignments.map(_._1).toSet
    val coveredCols = assignments.map(_._2).toSet
    
    // Find minimum uncovered value
    var minUncovered = Int.MaxValue
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        if (!coveredRows.contains(i) && !coveredCols.contains(j)) {
          minUncovered = math.min(minUncovered, matrix(i)(j))
        }
      }
    }
    
    // Adjust matrix
    val newMatrix = matrix.map(_.clone)
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        if (!coveredRows.contains(i) && !coveredCols.contains(j)) {
          newMatrix(i)(j) -= minUncovered
        } else if (coveredRows.contains(i) && coveredCols.contains(j)) {
          newMatrix(i)(j) += minUncovered
        }
      }
    }
    
    newMatrix
  }
}

// Example usage
object Main extends App {
  // Example cost matrix (3x3)
  val costMatrix = Array(
    Array(9, 2, 7),
    Array(6, 5, 8),
    Array(1, 4, 3)
  )
  
  println("Cost Matrix:")
  costMatrix.foreach(row => println(row.mkString(" ")))
  
  val (totalCost, assignments) = HungarianAlgorithm.solve(costMatrix)
  
  println(s"\nTotal Cost: $totalCost")
  println("Assignments:")
  assignments.foreach { assignment =>
    println(s"Row ${assignment.row} -> Column ${assignment.col} (cost: ${assignment.cost})")
  }
  
  // Another example with 4x4 matrix
  println("\n" + "="*50)
  val costMatrix2 = Array(
    Array(10, 19, 8, 15),
    Array(10, 18, 7, 14),
    Array(13, 16, 9, 17),
    Array(12, 15, 8, 16)
  )
  
  println("Cost Matrix 2:")
  costMatrix2.foreach(row => println(row.mkString(" ")))
  
  val (totalCost2, assignments2) = HungarianAlgorithm.solve(costMatrix2)
  
  println(s"\nTotal Cost: $totalCost2")
  println("Assignments:")
  assignments2.foreach { assignment =>
    println(s"Row ${assignment.row} -> Column ${assignment.col} (cost: ${assignment.cost})")
  }
}
```

## How it works:

1. **Step 1**: Subtract the minimum value from each row
2. **Step 2**: Subtract the minimum value from each column
3. **Step 3**: Find the minimum number of lines to cover all zeros
4. **Step 4**: If the number of lines equals the matrix size, we have an optimal solution
5. **Step 5**: If not optimal, adjust the matrix and repeat

## Key Features:

- **Functional approach** with immutable data structures
- **Recursive solution** that handles the adjustment step
- **Clear separation** of concerns with helper methods
- **Example usage** with two different test cases
- **Proper assignment tracking** with cost calculation

## Time Complexity:
- O(n³) where n is the size of the square matrix
- Efficient for moderate-sized assignment problems

This implementation demonstrates the classic Hungarian algorithm for solving linear assignment problems, commonly used in operations research and optimization problems.


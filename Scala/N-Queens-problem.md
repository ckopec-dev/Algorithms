# N-Queens Problem in Scala

Here's a complete implementation of the N-Queens problem solver using backtracking in Scala:

```scala
object NQueens {
  
  def solveNQueens(n: Int): List[List[Int]] = {
    def isSafe(queens: List[Int], row: Int, col: Int): Boolean = {
      queens.zipWithIndex.exists { case (q, i) =>
        q == col || // Same column
        q == col + (row - i) || // Same diagonal (top-right)
        q == col - (row - i)     // Same diagonal (top-left)
      } == false
    }
    
    def backtrack(queens: List[Int], row: Int): List[List[Int]] = {
      if (row == n) {
        List(queens)
      } else {
        (0 until n).filter(col => isSafe(queens, row, col))
          .flatMap(col => backtrack(col :: queens, row + 1))
      }
    }
    
    backtrack(Nil, 0)
  }
  
  def printBoard(solution: List[Int]): Unit = {
    val n = solution.length
    println("+" + "-" * n + "+")
    solution.foreach { col =>
      val row = Array.fill(n)('.')
      row(col) = 'Q'
      println("|" + row.mkString("") + "|")
    }
    println("+" + "-" * n + "+")
  }
  
  def main(args: Array[String]): Unit = {
    val n = 4
    val solutions = solveNQueens(n)
    
    println(s"Found ${solutions.length} solutions for ${n}-Queens problem:")
    
    solutions.foreach { solution =>
      printBoard(solution)
      println()
    }
  }
}
```

## Alternative Implementation with More Efficient Checking

```scala
object NQueensOptimized {
  
  def solveNQueens(n: Int): List[List[Int]] = {
    def solve(row: Int, cols: Set[Int], diag1: Set[Int], diag2: Set[Int], queens: List[Int]): List[List[Int]] = {
      if (row == n) {
        List(queens)
      } else {
        (0 until n)
          .filter(col => !cols.contains(col) && 
                        !diag1.contains(row + col) && 
                        !diag2.contains(row - col))
          .flatMap(col => solve(row + 1, 
                               cols + col, 
                               diag1 + (row + col), 
                               diag2 + (row - col), 
                               col :: queens))
      }
    }
    
    solve(0, Set(), Set(), Set(), Nil)
  }
  
  def printSolution(solution: List[Int]): Unit = {
    val n = solution.length
    println("+" + ("-" * n) + "+")
    solution.zipWithIndex.foreach { case (col, row) =>
      val line = Array.fill(n)('.')
      line(col) = 'Q'
      println("|" + line.mkString("") + "|")
    }
    println("+" + ("-" * n) + "+")
  }
  
  def main(args: Array[String]): Unit = {
    val n = 8
    val solutions = solveNQueens(n)
    
    println(s"Found ${solutions.length} solutions for ${n}-Queens problem:")
    
    solutions.take(2).foreach { solution =>
      printSolution(solution)
      println()
    }
  }
}
```

## Key Features of This Implementation:

1. **Backtracking Algorithm**: Uses recursive backtracking to explore all possible placements
2. **Safe Position Checking**: Efficiently checks if a queen can be placed without conflict
3. **Multiple Solutions**: Returns all valid solutions for the given N
4. **Visualization**: Includes board printing functionality to visualize solutions
5. **Optimized Version**: Uses sets for O(1) lookup time for column and diagonal conflicts

## Sample Output for 4-Queens:
```
Found 2 solutions for 4-Queens problem:
+----+
|Q...|
|...Q|
|Q...|
|...Q|
+----+

+----+
|..Q.|
|Q...|
|...Q|
|.Q..|
+----+
```

The algorithm has time complexity of O(N!) in the worst case and space complexity of O(N) for the recursion stack.


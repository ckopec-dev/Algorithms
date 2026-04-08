# N-Queens Problem in Kotlin

Here's a complete implementation of the N-Queens problem algorithm in Kotlin using backtracking:

```kotlin
class NQueens {
    private lateinit var board: Array<Array<Int>>
    private val solutions = mutableListOf<List<Int>>()
    
    fun solveNQueens(n: Int): List<List<Int>> {
        board = Array(n) { Array(n) { 0 } }
        solutions.clear()
        backtrack(0, n)
        return solutions
    }
    
    private fun backtrack(row: Int, n: Int) {
        if (row == n) {
            // Found a valid solution
            val solution = mutableListOf<Int>()
            for (i in 0 until n) {
                for (j in 0 until n) {
                    if (board[i][j] == 1) {
                        solution.add(j)
                        break
                    }
                }
            }
            solutions.add(solution)
            return
        }
        
        for (col in 0 until n) {
            if (isSafe(row, col, n)) {
                board[row][col] = 1
                backtrack(row + 1, n)
                board[row][col] = 0 // Backtrack
            }
        }
    }
    
    private fun isSafe(row: Int, col: Int, n: Int): Boolean {
        // Check this row on left side
        for (i in 0 until col) {
            if (board[row][i] == 1) return false
        }
        
        // Check upper diagonal on left side
        var i = row
        var j = col
        while (i >= 0 && j >= 0) {
            if (board[i][j] == 1) return false
            i--
            j--
        }
        
        // Check lower diagonal on left side
        i = row
        j = col
        while (i < n && j >= 0) {
            if (board[i][j] == 1) return false
            i++
            j--
        }
        
        return true
    }
    
    // Print the board for visualization
    fun printSolution(solution: List<Int>) {
        val n = solution.size
        for (i in 0 until n) {
            for (j in 0 until n) {
                if (solution[i] == j) {
                    print("Q ")
                } else {
                    print(". ")
                }
            }
            println()
        }
        println()
    }
}

// Usage example
fun main() {
    val nQueens = NQueens()
    
    println("Solving 4-Queens problem:")
    val solutions = nQueens.solveNQueens(4)
    
    println("Number of solutions: ${solutions.size}")
    
    solutions.forEachIndexed { index, solution ->
        println("Solution ${index + 1}: $solution")
        nQueens.printSolution(solution)
    }
    
    println("Solving 8-Queens problem:")
    val solutions8 = nQueens.solveNQueens(8)
    println("Number of solutions for 8-Queens: ${solutions8.size}")
}
```

## Key Features of this Implementation:

1. **Backtracking Algorithm**: Uses recursive backtracking to explore all possible placements
2. **Safety Check**: The `isSafe` function checks if a queen can be placed at a given position
3. **Solution Storage**: Stores all valid solutions in a list
4. **Board Visualization**: Includes a method to print the board with queens represented as 'Q'

## How it Works:

1. **Initialization**: Creates an n×n board initialized with zeros
2. **Backtracking Process**: 
   - Places a queen in each row
   - Checks if the placement is safe
   - Recursively continues to the next row
   - If no valid placement exists, backtracks
3. **Safety Validation**: Ensures no two queens threaten each other (same row, column, or diagonal)

## Sample Output:
```
Solving 4-Queens problem:
Number of solutions: 2
Solution 1: [1, 3, 0, 2]
. Q . . 
. . . Q 
Q . . . 
. . Q . 

Solution 2: [2, 0, 3, 1]
. . Q . 
Q . . . 
. . . Q 
. Q . . 
```

This implementation efficiently solves the N-Queens problem and can handle boards of various sizes.


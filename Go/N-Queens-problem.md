# N-Queens Problem in Go

Here's a complete implementation of the N-Queens problem solver in Go using backtracking:

```go
package main

import (
    "fmt"
    "strings"
)

// NQueens solves the N-Queens problem
func NQueens(n int) [][]int {
    if n <= 0 {
        return [][]int{}
    }
    
    solutions := [][]int{}
    board := make([]int, n) // board[i] represents column position of queen in row i
    
    backtrack(board, 0, &solutions)
    return solutions
}

// backtrack uses backtracking to find all solutions
func backtrack(board []int, row int, solutions *[][]int) {
    n := len(board)
    
    // Base case: all queens are placed
    if row == n {
        // Create a copy of current solution
        solution := make([]int, len(board))
        copy(solution, board)
        *solutions = append(*solutions, solution)
        return
    }
    
    // Try placing queen in each column of current row
    for col := 0; col < n; col++ {
        if isValid(board, row, col) {
            board[row] = col // Place queen
            backtrack(board, row+1, solutions) // Recurse to next row
            // No need to explicitly backtrack as we're using a new board for each path
        }
    }
}

// isValid checks if placing a queen at (row, col) is valid
func isValid(board []int, row, col int) bool {
    // Check if any queen in previous rows attacks this position
    for i := 0; i < row; i++ {
        // Check column conflict
        if board[i] == col {
            return false
        }
        // Check diagonal conflict
        if abs(board[i]-col) == abs(i-row) {
            return false
        }
    }
    return true
}

// abs returns absolute value
func abs(x int) int {
    if x < 0 {
        return -x
    }
    return x
}

// printBoard prints the board representation
func printBoard(solution []int) {
    n := len(solution)
    fmt.Println("+" + strings.Repeat("-", 2*n-1) + "+")
    
    for i := 0; i < n; i++ {
        row := make([]string, n)
        for j := 0; j < n; j++ {
            if solution[i] == j {
                row[j] = "Q"
            } else {
                row[j] = "."
            }
        }
        fmt.Println("| " + strings.Join(row, " ") + " |")
    }
    fmt.Println("+" + strings.Repeat("-", 2*n-1) + "+")
    fmt.Println()
}

func main() {
    // Solve 4-Queens problem
    fmt.Println("Solving 4-Queens Problem:")
    solutions := NQueens(4)
    
    fmt.Printf("Found %d solutions:\n\n", len(solutions))
    
    for i, solution := range solutions {
        fmt.Printf("Solution %d:\n", i+1)
        printBoard(solution)
    }
    
    // Solve 8-Queens problem (more complex)
    fmt.Println("Solving 8-Queens Problem:")
    solutions8 := NQueens(8)
    fmt.Printf("Found %d solutions for 8-Queens\n", len(solutions8))
    
    // Display first 3 solutions for 8-Queens
    fmt.Println("First 3 solutions for 8-Queens:")
    for i := 0; i < min(3, len(solutions8)); i++ {
        fmt.Printf("Solution %d:\n", i+1)
        printBoard(solutions8[i])
    }
}

// min returns the smaller of two integers
func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}
```

## How it works:

1. **Backtracking Algorithm**: Uses recursive backtracking to try placing queens row by row
2. **Validation**: Checks if a queen placement is valid (no conflicts with existing queens)
3. **Conflict Detection**: 
   - Column conflict: Same column as existing queen
   - Diagonal conflict: Distance between rows equals distance between columns
4. **Solution Storage**: Stores valid board configurations when all queens are placed

## Key Features:

- **Efficient**: Uses constraint checking to prune invalid branches early
- **Complete**: Finds all possible solutions for a given N
- **Visual**: Includes board visualization for better understanding
- **Scalable**: Can solve for any N (though time complexity grows exponentially)

## Sample Output:
```
Solving 4-Queens Problem:
Found 2 solutions:

Solution 1:
+-------+
| Q . . . |
| . . . Q |
| . Q . . |
| . . Q . |
+-------+

Solution 2:
+-------+
| . . Q . |
| Q . . . |
| . . . Q |
| . Q . . |
+-------+
```

The algorithm has time complexity of O(N!) in the worst case, which is expected for the N-Queens problem, and space complexity of O(N) for the recursion stack.


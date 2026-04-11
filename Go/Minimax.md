# Minimax Algorithm in Go

Here's a complete implementation of the Minimax algorithm in Go for a simple tic-tac-toe game:

```go
package main

import (
    "fmt"
    "math"
)

// Game state representation
type Board struct {
    grid [3][3]string
}

// Game state constants
const (
    EMPTY = ""
    X     = "X"
    O     = "O"
)

// Initialize empty board
func NewBoard() *Board {
    return &Board{
        grid: [3][3]string{
            {EMPTY, EMPTY, EMPTY},
            {EMPTY, EMPTY, EMPTY},
            {EMPTY, EMPTY, EMPTY},
        },
    }
}

// Check if board is full
func (b *Board) IsFull() bool {
    for i := 0; i < 3; i++ {
        for j := 0; j < 3; j++ {
            if b.grid[i][j] == EMPTY {
                return false
            }
        }
    }
    return true
}

// Check if a player has won
func (b *Board) IsWin(player string) bool {
    // Check rows
    for i := 0; i < 3; i++ {
        if b.grid[i][0] == player && b.grid[i][1] == player && b.grid[i][2] == player {
            return true
        }
    }
    
    // Check columns
    for j := 0; j < 3; j++ {
        if b.grid[0][j] == player && b.grid[1][j] == player && b.grid[2][j] == player {
            return true
        }
    }
    
    // Check diagonals
    if b.grid[0][0] == player && b.grid[1][1] == player && b.grid[2][2] == player {
        return true
    }
    if b.grid[0][2] == player && b.grid[1][1] == player && b.grid[2][0] == player {
        return true
    }
    
    return false
}

// Get available moves
func (b *Board) GetMoves() [][2]int {
    var moves [][2]int
    for i := 0; i < 3; i++ {
        for j := 0; j < 3; j++ {
            if b.grid[i][j] == EMPTY {
                moves = append(moves, [2]int{i, j})
            }
        }
    }
    return moves
}

// Make a move
func (b *Board) MakeMove(row, col int, player string) {
    b.grid[row][col] = player
}

// Undo a move
func (b *Board) UndoMove(row, col int) {
    b.grid[row][col] = EMPTY
}

// Evaluate the board state
func (b *Board) Evaluate() int {
    if b.IsWin(X) {
        return 10
    } else if b.IsWin(O) {
        return -10
    } else {
        return 0
    }
}

// Minimax algorithm implementation
func minimax(board *Board, depth int, isMaximizing bool) int {
    score := board.Evaluate()
    
    // Base cases
    if score == 10 { // X wins
        return score - depth
    }
    if score == -10 { // O wins
        return score + depth
    }
    if board.IsFull() { // Tie
        return 0
    }
    
    if isMaximizing {
        // Maximizer's turn (X)
        best := math.MinInt32
        moves := board.GetMoves()
        
        for _, move := range moves {
            row, col := move[0], move[1]
            board.MakeMove(row, col, X)
            value := minimax(board, depth+1, false)
            board.UndoMove(row, col)
            if value > best {
                best = value
            }
        }
        return best
    } else {
        // Minimizer's turn (O)
        best := math.MaxInt32
        moves := board.GetMoves()
        
        for _, move := range moves {
            row, col := move[0], move[1]
            board.MakeMove(row, col, O)
            value := minimax(board, depth+1, true)
            board.UndoMove(row, col)
            if value < best {
                best = value
            }
        }
        return best
    }
}

// Find best move for the AI
func findBestMove(board *Board) [2]int {
    bestVal := math.MinInt32
    bestMove := [2]int{-1, -1}
    
    moves := board.GetMoves()
    
    for _, move := range moves {
        row, col := move[0], move[1]
        board.MakeMove(row, col, X)
        moveVal := minimax(board, 0, false)
        board.UndoMove(row, col)
        
        if moveVal > bestVal {
            bestVal = moveVal
            bestMove = move
        }
    }
    
    return bestMove
}

// Print the board
func (b *Board) Print() {
    for i := 0; i < 3; i++ {
        for j := 0; j < 3; j++ {
            if b.grid[i][j] == EMPTY {
                fmt.Print(" - ")
            } else {
                fmt.Printf(" %s ", b.grid[i][j])
            }
        }
        fmt.Println()
    }
    fmt.Println()
}

func main() {
    board := NewBoard()
    
    fmt.Println("Tic-Tac-Toe Game")
    fmt.Println("You are O, AI is X")
    fmt.Println()
    
    // Game loop
    for !board.IsFull() {
        board.Print()
        
        // Human player's turn (O)
        fmt.Println("Enter your move (row and column, 0-2):")
        var row, col int
        fmt.Scanf("%d %d", &row, &col)
        
        if row < 0 || row > 2 || col < 0 || col > 2 {
            fmt.Println("Invalid move! Try again.")
            continue
        }
        
        if board.grid[row][col] != EMPTY {
            fmt.Println("Position already taken! Try again.")
            continue
        }
        
        board.MakeMove(row, col, O)
        
        if board.IsWin(O) {
            board.Print()
            fmt.Println("You win!")
            return
        }
        
        if board.IsFull() {
            board.Print()
            fmt.Println("It's a tie!")
            return
        }
        
        // AI's turn (X)
        fmt.Println("AI is thinking...")
        bestMove := findBestMove(board)
        board.MakeMove(bestMove[0], bestMove[1], X)
        
        if board.IsWin(X) {
            board.Print()
            fmt.Println("AI wins!")
            return
        }
    }
    
    board.Print()
    fmt.Println("It's a tie!")
}
```

## Key Components Explained

### 1. **Board Structure**
- 3x3 grid represented as `[3][3]string`
- Uses constants for EMPTY, X, and O

### 2. **Minimax Function**
- Recursively evaluates all possible game states
- Returns the best score for the current player
- Uses depth to favor quicker wins

### 3. **Evaluation Function**
- Returns 10 if X wins
- Returns -10 if O wins  
- Returns 0 for tie or ongoing game

### 4. **Game Logic**
- Alternates between human (O) and AI (X) turns
- Uses minimax to determine optimal AI moves
- Handles win/tie detection

## How It Works

1. **Minimax Algorithm**: Explores all possible moves recursively
2. **Alpha-Beta Pruning**: Can be added for optimization
3. **Depth Adjustment**: Prevents AI from choosing longer paths to win
4. **Move Selection**: Chooses the move with the highest minimax value

This implementation provides a solid foundation that can be extended for more complex games or optimized with additional features like alpha-beta pruning.


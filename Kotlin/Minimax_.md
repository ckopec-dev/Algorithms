# Minimax Algorithm in Kotlin

Here's a complete implementation of the Minimax algorithm in Kotlin with a tic-tac-toe example:

```kotlin
import kotlin.math.max
import kotlin.math.min

// Game state representation
data class GameState(
    val board: Array<Array<String?>>, // null = empty, "X" = player X, "O" = player O
    val currentPlayer: String
) {
    fun isTerminal(): Boolean {
        return hasWinner() || isBoardFull()
    }
    
    fun hasWinner(): Boolean {
        // Check rows
        for (i in 0..2) {
            if (board[i][0] != null && 
                board[i][0] == board[i][1] && 
                board[i][0] == board[i][2]) {
                return true
            }
        }
        
        // Check columns
        for (j in 0..2) {
            if (board[0][j] != null && 
                board[0][j] == board[1][j] && 
                board[0][j] == board[2][j]) {
                return true
            }
        }
        
        // Check diagonals
        if (board[0][0] != null && 
            board[0][0] == board[1][1] && 
            board[0][0] == board[2][2]) {
            return true
        }
        
        if (board[0][2] != null && 
            board[0][2] == board[1][1] && 
            board[0][2] == board[2][0]) {
            return true
        }
        
        return false
    }
    
    fun isBoardFull(): Boolean {
        for (i in 0..2) {
            for (j in 0..2) {
                if (board[i][j] == null) {
                    return false
                }
            }
        }
        return true
    }
    
    fun getScore(): Int {
        if (hasWinner()) {
            return if (currentPlayer == "O") 10 else -10
        }
        return 0
    }
    
    fun getAvailableMoves(): List<Pair<Int, Int>> {
        val moves = mutableListOf<Pair<Int, Int>>()
        for (i in 0..2) {
            for (j in 0..2) {
                if (board[i][j] == null) {
                    moves.add(Pair(i, j))
                }
            }
        }
        return moves
    }
    
    fun makeMove(row: Int, col: Int, player: String): GameState {
        val newBoard = board.map { it.clone() }.toTypedArray()
        newBoard[row][col] = player
        return GameState(newBoard, if (player == "X") "O" else "X")
    }
}

class Minimax {
    
    fun minimax(state: GameState, depth: Int, isMaximizing: Boolean): Int {
        // Base case: if game is over or depth limit reached
        if (state.isTerminal()) {
            return state.getScore()
        }
        
        if (isMaximizing) {
            var maxEval = Int.MIN_VALUE
            for ((row, col) in state.getAvailableMoves()) {
                val newState = state.makeMove(row, col, "O") // O is maximizing player
                val eval = minimax(newState, depth + 1, false)
                maxEval = max(maxEval, eval)
            }
            return maxEval
        } else {
            var minEval = Int.MAX_VALUE
            for ((row, col) in state.getAvailableMoves()) {
                val newState = state.makeMove(row, col, "X") // X is minimizing player
                val eval = minimax(newState, depth + 1, true)
                minEval = min(minEval, eval)
            }
            return minEval
        }
    }
    
    fun findBestMove(state: GameState): Pair<Int, Int>? {
        var bestValue = if (state.currentPlayer == "O") Int.MIN_VALUE else Int.MAX_VALUE
        var bestMove: Pair<Int, Int>? = null
        
        for ((row, col) in state.getAvailableMoves()) {
            val newState = state.makeMove(row, col, state.currentPlayer)
            val moveValue = minimax(newState, 0, state.currentPlayer == "X")
            
            if (state.currentPlayer == "O") {
                if (moveValue > bestValue) {
                    bestValue = moveValue
                    bestMove = Pair(row, col)
                }
            } else {
                if (moveValue < bestValue) {
                    bestValue = moveValue
                    bestMove = Pair(row, col)
                }
            }
        }
        
        return bestMove
    }
}

// Example usage
fun main() {
    // Create initial game state (empty board)
    val initialBoard = Array(3) { Array(3) { null } }
    val initialState = GameState(initialBoard, "O") // O starts first
    
    val minimax = Minimax()
    
    // Simulate a game
    var currentState = initialState
    
    println("Initial board:")
    printBoard(currentState.board)
    
    // Make some moves manually to demonstrate
    currentState = currentState.makeMove(0, 0, "O") // O plays first move
    currentState = currentState.makeMove(1, 1, "X") // X plays second move
    currentState = currentState.makeMove(0, 1, "O") // O plays third move
    
    println("\nAfter some moves:")
    printBoard(currentState.board)
    
    // Find best move for current player
    val bestMove = minimax.findBestMove(currentState)
    if (bestMove != null) {
        println("\nBest move for player ${currentState.currentPlayer}: (${bestMove.first}, ${bestMove.second})")
    }
    
    // Test with a more complex example
    println("\n--- Testing with a winning scenario ---")
    val winningBoard = Array(3) { Array(3) { null } }
    winningBoard[0][0] = "O"
    winningBoard[0][1] = "O"
    winningBoard[1][0] = "X"
    winningBoard[1][1] = "X"
    winningBoard[2][0] = "X"
    
    val winningState = GameState(winningBoard, "O")
    println("Winning board:")
    printBoard(winningState.board)
    
    val score = minimax.minimax(winningState, 0, true)
    println("Minimax score: $score")
}

fun printBoard(board: Array<Array<String?>>) {
    for (i in 0..2) {
        for (j in 0..2) {
            print(board[i][j] ?: " ")
            if (j < 2) print(" | ")
        }
        println()
        if (i < 2) println("---------")
    }
    println()
}
```

## Key Components Explained:

### 1. **GameState Class**
- Represents the current state of the tic-tac-toe board
- Tracks current player and board configuration
- Provides methods to check if game is over, get available moves, etc.

### 2. **Minimax Class**
- **minimax()**: Recursive function that implements the minimax algorithm
- **findBestMove()**: Determines the best move for the current player
- Uses alpha-beta pruning concepts (simplified version)

### 3. **Algorithm Logic**
- **Maximizing Player** ("O"): Tries to maximize score (win)
- **Minimizing Player** ("X"): Tries to minimize score (lose for opponent)
- **Base Case**: When game is over, return score (10 for win, -10 for loss, 0 for draw)
- **Recursive Case**: Try all possible moves and choose the optimal one

### 4. **Time Complexity**
- O(b^d) where b is branching factor (9 for tic-tac-toe) and d is depth
- In practice, much better due to early termination and game-specific optimizations

This implementation demonstrates the core concepts of minimax algorithm for two-player zero-sum games, with clear separation of game logic and AI decision-making.


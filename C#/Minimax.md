# Minimax Algorithm in C#

Here's a complete implementation of the Minimax algorithm in C# with a simple tic-tac-toe game example:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class MinimaxExample
{
    // Game board represented as a 3x3 array
    private char[,] board = new char[3, 3];
    
    // Player symbols
    private const char PLAYER_X = 'X';
    private const char PLAYER_O = 'O';
    private const char EMPTY = ' ';
    
    public MinimaxExample()
    {
        // Initialize empty board
        InitializeBoard();
    }
    
    private void InitializeBoard()
    {
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                board[i, j] = EMPTY;
            }
        }
    }
    
    // Check if the board is full
    private bool IsBoardFull()
    {
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if (board[i, j] == EMPTY)
                    return false;
            }
        }
        return true;
    }
    
    // Check if a player has won
    private bool IsWinner(char player)
    {
        // Check rows
        for (int i = 0; i < 3; i++)
        {
            if (board[i, 0] == player && board[i, 1] == player && board[i, 2] == player)
                return true;
        }
        
        // Check columns
        for (int j = 0; j < 3; j++)
        {
            if (board[0, j] == player && board[1, j] == player && board[2, j] == player)
                return true;
        }
        
        // Check diagonals
        if (board[0, 0] == player && board[1, 1] == player && board[2, 2] == player)
            return true;
        
        if (board[0, 2] == player && board[1, 1] == player && board[2, 0] == player)
            return true;
            
        return false;
    }
    
    // Get all available moves
    private List<(int, int)> GetAvailableMoves()
    {
        var moves = new List<(int, int)>();
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if (board[i, j] == EMPTY)
                    moves.Add((i, j));
            }
        }
        return moves;
    }
    
    // Evaluate the current board state
    private int EvaluateBoard()
    {
        if (IsWinner(PLAYER_X))
            return 10;  // X wins
        else if (IsWinner(PLAYER_O))
            return -10; // O wins
        else
            return 0;   // Draw or game not finished
    }
    
    // Minimax algorithm implementation
    public int Minimax(int depth, bool isMaximizing, int alpha, int beta)
    {
        int score = EvaluateBoard();
        
        // Base cases
        if (score == 10) return score - depth;   // X wins
        if (score == -10) return score + depth;  // O wins
        if (IsBoardFull()) return 0;             // Draw
        
        if (isMaximizing)
        {
            int maxEval = int.MinValue;
            
            // Try all possible moves
            foreach (var move in GetAvailableMoves())
            {
                int row = move.Item1;
                int col = move.Item2;
                
                // Make the move
                board[row, col] = PLAYER_X;
                
                // Recursively call minimax
                int eval = Minimax(depth + 1, false, alpha, beta);
                
                // Undo the move
                board[row, col] = EMPTY;
                
                // Update maximum evaluation
                maxEval = Math.Max(maxEval, eval);
                alpha = Math.Max(alpha, eval);
                
                // Alpha-beta pruning
                if (beta <= alpha)
                    break;
            }
            
            return maxEval;
        }
        else
        {
            int minEval = int.MaxValue;
            
            // Try all possible moves
            foreach (var move in GetAvailableMoves())
            {
                int row = move.Item1;
                int col = move.Item2;
                
                // Make the move
                board[row, col] = PLAYER_O;
                
                // Recursively call minimax
                int eval = Minimax(depth + 1, true, alpha, beta);
                
                // Undo the move
                board[row, col] = EMPTY;
                
                // Update minimum evaluation
                minEval = Math.Min(minEval, eval);
                beta = Math.Min(beta, eval);
                
                // Alpha-beta pruning
                if (beta <= alpha)
                    break;
            }
            
            return minEval;
        }
    }
    
    // Find the best move for the maximizing player (X)
    public (int, int) FindBestMove()
    {
        int bestValue = int.MinValue;
        (int, int) bestMove = (-1, -1);
        
        foreach (var move in GetAvailableMoves())
        {
            int row = move.Item1;
            int col = move.Item2;
            
            // Make the move
            board[row, col] = PLAYER_X;
            
            // Calculate minimax value
            int moveValue = Minimax(0, false, int.MinValue, int.MaxValue);
            
            // Undo the move
            board[row, col] = EMPTY;
            
            // Update best move
            if (moveValue > bestValue)
            {
                bestValue = moveValue;
                bestMove = (row, col);
            }
        }
        
        return bestMove;
    }
    
    // Display the current board
    public void DisplayBoard()
    {
        Console.WriteLine("Current Board:");
        for (int i = 0; i < 3; i++)
        {
            Console.Write("| ");
            for (int j = 0; j < 3; j++)
            {
                Console.Write(board[i, j] + " | ");
            }
            Console.WriteLine();
            Console.WriteLine("-------------");
        }
        Console.WriteLine();
    }
    
    // Main method to demonstrate the algorithm
    public static void Main(string[] args)
    {
        MinimaxExample game = new MinimaxExample();
        
        Console.WriteLine("Minimax Algorithm Demo - Tic-Tac-Toe");
        Console.WriteLine("=====================================");
        
        // Example 1: Game state where X should make the best move
        game.board[0, 0] = PLAYER_X;
        game.board[0, 1] = PLAYER_O;
        game.board[0, 2] = PLAYER_O;
        game.board[1, 0] = PLAYER_O;
        game.board[1, 1] = PLAYER_X;
        game.board[1, 2] = PLAYER_X;
        game.board[2, 0] = PLAYER_X;
        game.board[2, 1] = PLAYER_O;
        game.board[2, 2] = PLAYER_O;
        
        game.DisplayBoard();
        
        var bestMove = game.FindBestMove();
        Console.WriteLine($"Best move for X: Row {bestMove.Item1}, Column {bestMove.Item2}");
        
        // Example 2: Empty board
        game.InitializeBoard();
        Console.WriteLine("\nEmpty board:");
        game.DisplayBoard();
        
        bestMove = game.FindBestMove();
        Console.WriteLine($"Best move for X: Row {bestMove.Item1}, Column {bestMove.Item2}");
        
        Console.WriteLine("\nMinimax algorithm implemented successfully!");
    }
}
```

## Key Features of this Implementation:

### 1. **Core Minimax Logic**
- Recursive algorithm that evaluates all possible game states
- Alternates between maximizing (X) and minimizing (O) players
- Uses alpha-beta pruning for optimization

### 2. **Alpha-Beta Pruning**
- Implements pruning to reduce unnecessary calculations
- Alpha represents the best value the maximizing player can guarantee
- Beta represents the best value the minimizing player can guarantee

### 3. **Evaluation Function**
- Returns 10 for X win
- Returns -10 for O win  
- Returns 0 for draw or ongoing game

### 4. **Game State Management**
- Board representation as 3x3 char array
- Move validation and undo functionality
- Win condition checking for rows, columns, and diagonals

### 5. **Optimization**
- Prunes branches that won't affect the final decision
- Efficient move generation and evaluation
- Proper depth management

This implementation demonstrates how Minimax works in a real game scenario and can be easily adapted for other games like chess, connect-four, or any two-player deterministic game.


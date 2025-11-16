# Minimax Algorithm in C++

Here's a complete implementation of the Minimax algorithm for a simple game (Tic-Tac-Toe):

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

class TicTacToe {
private:
    vector<vector<char>> board;
    static const char EMPTY = ' ';
    static const char PLAYER_X = 'X';
    static const char PLAYER_O = 'O';

public:
    TicTacToe() {
        board = vector<vector<char>>(3, vector<char>(3, EMPTY));
    }

    // Check if the game is over
    bool isGameOver() {
        return isWin(PLAYER_X) || isWin(PLAYER_O) || isBoardFull();
    }

    // Check if a player has won
    bool isWin(char player) {
        // Check rows
        for (int i = 0; i < 3; i++) {
            if (board[i][0] == player && board[i][1] == player && board[i][2] == player)
                return true;
        }
        
        // Check columns
        for (int j = 0; j < 3; j++) {
            if (board[0][j] == player && board[1][j] == player && board[2][j] == player)
                return true;
        }
        
        // Check diagonals
        if (board[0][0] == player && board[1][1] == player && board[2][2] == player)
            return true;
        if (board[0][2] == player && board[1][1] == player && board[2][0] == player)
            return true;
            
        return false;
    }

    // Check if board is full
    bool isBoardFull() {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[i][j] == EMPTY)
                    return false;
            }
        }
        return true;
    }

    // Get available moves
    vector<pair<int, int>> getAvailableMoves() {
        vector<pair<int, int>> moves;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[i][j] == EMPTY) {
                    moves.push_back({i, j});
                }
            }
        }
        return moves;
    }

    // Make a move
    void makeMove(int row, int col, char player) {
        board[row][col] = player;
    }

    // Undo a move
    void undoMove(int row, int col) {
        board[row][col] = EMPTY;
    }

    // Print the board
    void printBoard() {
        cout << "\n  0   1   2\n";
        for (int i = 0; i < 3; i++) {
            cout << i << " ";
            for (int j = 0; j < 3; j++) {
                cout << board[i][j];
                if (j < 2) cout << " | ";
            }
            cout << "\n";
            if (i < 2) cout << "  ---------\n";
        }
        cout << "\n";
    }

    // Minimax algorithm
    int minimax(char player, int depth, bool isMaximizing) {
        // Base cases
        if (isWin(PLAYER_X)) return 10 - depth;
        if (isWin(PLAYER_O)) return depth - 10;
        if (isBoardFull()) return 0;

        if (isMaximizing) {
            int bestScore = INT_MIN;
            vector<pair<int, int>> moves = getAvailableMoves();
            
            for (auto move : moves) {
                int row = move.first;
                int col = move.second;
                makeMove(row, col, player);
                int score = minimax(PLAYER_O, depth + 1, false);
                undoMove(row, col);
                bestScore = max(bestScore, score);
            }
            return bestScore;
        } else {
            int bestScore = INT_MAX;
            vector<pair<int, int>> moves = getAvailableMoves();
            
            for (auto move : moves) {
                int row = move.first;
                int col = move.second;
                makeMove(row, col, player);
                int score = minimax(PLAYER_X, depth + 1, true);
                undoMove(row, col);
                bestScore = min(bestScore, score);
            }
            return bestScore;
        }
    }

    // Find best move for AI
    pair<int, int> findBestMove() {
        int bestScore = INT_MIN;
        pair<int, int> bestMove = {-1, -1};
        
        vector<pair<int, int>> moves = getAvailableMoves();
        
        for (auto move : moves) {
            int row = move.first;
            int col = move.second;
            makeMove(row, col, PLAYER_X);
            int score = minimax(PLAYER_O, 0, false);
            undoMove(row, col);
            
            if (score > bestScore) {
                bestScore = score;
                bestMove = move;
            }
        }
        
        return bestMove;
    }
};

int main() {
    TicTacToe game;
    char currentPlayer = 'X'; // Human player
    
    cout << "Tic-Tac-Toe Game with Minimax AI\n";
    cout << "You are 'O', AI is 'X'\n";
    
    while (!game.isGameOver()) {
        game.printBoard();
        
        if (currentPlayer == 'O') {
            // Human's turn
            int row, col;
            cout << "Enter your move (row col): ";
            cin >> row >> col;
            
            if (row >= 0 && row < 3 && col >= 0 && col < 3 && 
                game.getAvailableMoves().size() > 0) {
                game.makeMove(row, col, currentPlayer);
                currentPlayer = 'X';
            } else {
                cout << "Invalid move! Try again.\n";
            }
        } else {
            // AI's turn
            cout << "AI is thinking...\n";
            pair<int, int> bestMove = game.findBestMove();
            game.makeMove(bestMove.first, bestMove.second, currentPlayer);
            cout << "AI plays at (" << bestMove.first << ", " << bestMove.second << ")\n";
            currentPlayer = 'O';
        }
    }
    
    game.printBoard();
    
    if (game.isWin('X')) {
        cout << "AI wins!\n";
    } else if (game.isWin('O')) {
        cout << "You win!\n";
    } else {
        cout << "It's a tie!\n";
    }
    
    return 0;
}
```

## Key Components of the Minimax Implementation:

### 1. **Game State Representation**
- 3x3 board using `vector<vector<char>>`
- Players represented as 'X' (AI) and 'O' (human)

### 2. **Minimax Function**
- **Recursive**: Evaluates all possible game states
- **Alternating turns**: Maximizing (AI) and minimizing (human) players
- **Depth tracking**: Prevents longer games from being favored
- **Heuristic scoring**: 
  - Win: +10 - depth (favors quicker wins)
  - Loss: depth - 10 (favors slower losses)
  - Tie: 0

### 3. **Algorithm Logic**
- **Maximizing player** (AI): Chooses move that maximizes score
- **Minimizing player** (human): Chooses move that minimizes score
- **Backtracking**: Explores all possible moves and their outcomes

### 4. **Optimization Features**
- **Base cases**: Win, loss, or tie conditions
- **Move ordering**: Evaluates all available moves
- **State restoration**: Uses `makeMove` and `undoMove` for recursion

This implementation demonstrates how Minimax works by exploring the entire game tree and choosing the optimal move based on perfect play from both sides.


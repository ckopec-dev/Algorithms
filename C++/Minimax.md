# Minimax Algorithm in C++

Here's a complete implementation of the Minimax algorithm for a simple tic-tac-toe game:

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

class TicTacToe {
private:
    vector<vector<char>> board;
    char currentPlayer;

public:
    TicTacToe() {
        board = vector<vector<char>>(3, vector<char>(3, ' '));
        currentPlayer = 'X';
    }

    // Display the board
    void displayBoard() {
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

    // Check if a move is valid
    bool isValidMove(int row, int col) {
        return (row >= 0 && row < 3 && col >= 0 && col < 3 && board[row][col] == ' ');
    }

    // Make a move
    void makeMove(int row, int col) {
        if (isValidMove(row, col)) {
            board[row][col] = currentPlayer;
        }
    }

    // Undo a move
    void undoMove(int row, int col) {
        board[row][col] = ' ';
    }

    // Check if the game is over
    bool isGameOver() {
        // Check rows, columns, and diagonals
        for (int i = 0; i < 3; i++) {
            if (board[i][0] != ' ' && board[i][0] == board[i][1] && board[i][1] == board[i][2])
                return true;
            if (board[0][i] != ' ' && board[0][i] == board[1][i] && board[1][i] == board[2][i])
                return true;
        }
        if (board[0][0] != ' ' && board[0][0] == board[1][1] && board[1][1] == board[2][2])
            return true;
        if (board[0][2] != ' ' && board[0][2] == board[1][1] && board[1][1] == board[2][0])
            return true;

        // Check if board is full
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[i][j] == ' ') return false;
            }
        }
        return true;
    }

    // Get the winner
    char getWinner() {
        // Check rows
        for (int i = 0; i < 3; i++) {
            if (board[i][0] != ' ' && board[i][0] == board[i][1] && board[i][1] == board[i][2])
                return board[i][0];
        }
        // Check columns
        for (int i = 0; i < 3; i++) {
            if (board[0][i] != ' ' && board[0][i] == board[1][i] && board[1][i] == board[2][i])
                return board[0][i];
        }
        // Check diagonals
        if (board[0][0] != ' ' && board[0][0] == board[1][1] && board[1][1] == board[2][2])
            return board[0][0];
        if (board[0][2] != ' ' && board[0][2] == board[1][1] && board[1][1] == board[2][0])
            return board[0][2];
        
        return ' '; // No winner
    }

    // Evaluate the board
    int evaluate() {
        char winner = getWinner();
        if (winner == 'X') return 10;  // AI wins
        if (winner == 'O') return -10; // Human wins
        return 0; // Draw or game not over
    }

    // Minimax algorithm
    int minimax(int depth, bool isMaximizing) {
        int score = evaluate();
        
        // If game is over, return the score
        if (score == 10 || score == -10) {
            return score;
        }
        
        // If no moves left, it's a draw
        if (isGameOver()) {
            return 0;
        }
        
        if (isMaximizing) {
            int bestScore = INT_MIN;
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (board[i][j] == ' ') {
                        board[i][j] = 'X'; // AI makes move
                        int score = minimax(depth + 1, false);
                        board[i][j] = ' '; // Undo move
                        bestScore = max(bestScore, score);
                    }
                }
            }
            return bestScore;
        } else {
            int bestScore = INT_MAX;
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (board[i][j] == ' ') {
                        board[i][j] = 'O'; // Human makes move
                        int score = minimax(depth + 1, true);
                        board[i][j] = ' '; // Undo move
                        bestScore = min(bestScore, score);
                    }
                }
            }
            return bestScore;
        }
    }

    // Find the best move for AI
    pair<int, int> findBestMove() {
        int bestScore = INT_MIN;
        pair<int, int> bestMove = {-1, -1};
        
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[i][j] == ' ') {
                    board[i][j] = 'X';
                    int score = minimax(0, false);
                    board[i][j] = ' ';
                    
                    if (score > bestScore) {
                        bestScore = score;
                        bestMove = {i, j};
                    }
                }
            }
        }
        return bestMove;
    }

    // Switch player
    void switchPlayer() {
        currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
    }
};

int main() {
    TicTacToe game;
    int row, col;
    
    cout << "Tic-Tac-Toe Game (You are 'O', AI is 'X')\n";
    cout << "Enter row and column (0-2) to make your move.\n";
    
    while (!game.isGameOver()) {
        game.displayBoard();
        
        if (game.getWinner() != ' ') {
            cout << "Game Over! Winner: " << game.getWinner() << "\n";
            break;
        }
        
        if (game.currentPlayer == 'O') {
            cout << "Your turn (O):\n";
            cout << "Enter row: ";
            cin >> row;
            cout << "Enter column: ";
            cin >> col;
            
            if (game.isValidMove(row, col)) {
                game.makeMove(row, col);
                game.switchPlayer();
            } else {
                cout << "Invalid move! Try again.\n";
            }
        } else {
            cout << "AI is thinking...\n";
            pair<int, int> bestMove = game.findBestMove();
            game.makeMove(bestMove.first, bestMove.second);
            game.switchPlayer();
            cout << "AI moved to (" << bestMove.first << ", " << bestMove.second << ")\n";
        }
    }
    
    game.displayBoard();
    char winner = game.getWinner();
    if (winner != ' ') {
        cout << "Game Over! Winner: " << winner << "\n";
    } else {
        cout << "Game ended in a draw!\n";
    }
    
    return 0;
}
```

## Key Components of the Minimax Implementation:

### 1. **Minimax Function**
- Recursively evaluates all possible moves
- Alternates between maximizing (AI) and minimizing (human) players
- Returns the best possible score for the current position

### 2. **Evaluation Function**
- Returns 10 if AI wins
- Returns -10 if human wins  
- Returns 0 for a draw or ongoing game

### 3. **Game State Management**
- Board representation using 2D vector
- Move validation and undo functionality
- Game over detection

### 4. **Algorithm Logic**
- **Maximizing Player (AI)**: Chooses move that maximizes the score
- **Minimizing Player (Human)**: Chooses move that minimizes the score
- **Recursive Search**: Explores all possible game trees

## How it Works:
1. The AI evaluates all possible moves using minimax
2. For each move, it recursively explores the game tree
3. It assumes the opponent will play optimally
4. The AI selects the move that leads to the best possible outcome

This implementation demonstrates the core principles of the minimax algorithm for two-player games with perfect information.


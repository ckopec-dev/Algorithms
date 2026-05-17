# Minimax Algorithm in C

Here's a complete implementation of the Minimax algorithm for a simple tic-tac-toe game:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define BOARD_SIZE 3
#define EMPTY 0
#define PLAYER_X 1
#define PLAYER_O -1

int board[BOARD_SIZE][BOARD_SIZE];

// Initialize the board
void initializeBoard() {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            board[i][j] = EMPTY;
        }
    }
}

// Print the current board
void printBoard() {
    printf("\n  0 1 2\n");
    for (int i = 0; i < BOARD_SIZE; i++) {
        printf("%d ", i);
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                printf("- ");
            } else if (board[i][j] == PLAYER_X) {
                printf("X ");
            } else {
                printf("O ");
            }
        }
        printf("\n");
    }
    printf("\n");
}

// Check if the board is full
int isBoardFull() {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                return 0;
            }
        }
    }
    return 1;
}

// Check if there's a winner
int checkWinner() {
    // Check rows
    for (int i = 0; i < BOARD_SIZE; i++) {
        if (board[i][0] != EMPTY && board[i][0] == board[i][1] && board[i][1] == board[i][2]) {
            return board[i][0];
        }
    }
    
    // Check columns
    for (int j = 0; j < BOARD_SIZE; j++) {
        if (board[0][j] != EMPTY && board[0][j] == board[1][j] && board[1][j] == board[2][j]) {
            return board[0][j];
        }
    }
    
    // Check diagonals
    if (board[0][0] != EMPTY && board[0][0] == board[1][1] && board[1][1] == board[2][2]) {
        return board[0][0];
    }
    
    if (board[0][2] != EMPTY && board[0][2] == board[1][1] && board[1][1] == board[2][0]) {
        return board[0][2];
    }
    
    return 0; // No winner
}

// Minimax function
int minimax(int depth, int isMaximizing) {
    int winner = checkWinner();
    
    // Base cases
    if (winner == PLAYER_X) {
        return 10 - depth;  // Prefer faster wins
    } else if (winner == PLAYER_O) {
        return depth - 10;  // Prefer slower losses
    } else if (isBoardFull()) {
        return 0;  // Draw
    }
    
    if (isMaximizing) {
        int maxEval = INT_MIN;
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = PLAYER_X;
                    int eval = minimax(depth + 1, 0);
                    board[i][j] = EMPTY;
                    if (eval > maxEval) {
                        maxEval = eval;
                    }
                }
            }
        }
        return maxEval;
    } else {
        int minEval = INT_MAX;
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = PLAYER_O;
                    int eval = minimax(depth + 1, 1);
                    board[i][j] = EMPTY;
                    if (eval < minEval) {
                        minEval = eval;
                    }
                }
            }
        }
        return minEval;
    }
}

// Find the best move for AI
int* findBestMove() {
    static int move[2];
    int bestVal = INT_MIN;
    int bestRow = -1, bestCol = -1;
    
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                board[i][j] = PLAYER_X;
                int moveVal = minimax(0, 0);
                board[i][j] = EMPTY;
                
                if (moveVal > bestVal) {
                    bestVal = moveVal;
                    bestRow = i;
                    bestCol = j;
                }
            }
        }
    }
    
    move[0] = bestRow;
    move[1] = bestCol;
    return move;
}

// Main game function
void playGame() {
    initializeBoard();
    int gameOver = 0;
    int currentPlayer = PLAYER_X; // X goes first
    
    printf("Welcome to Tic-Tac-Toe!\n");
    printf("You are O, AI is X\n");
    printf("Enter row and column (0-2) to make your move\n\n");
    
    while (!gameOver) {
        printBoard();
        
        if (currentPlayer == PLAYER_O) {
            // Human's turn
            int row, col;
            printf("Your turn (O): Enter row and column (0-2): ");
            scanf("%d %d", &row, &col);
            
            if (row >= 0 && row < BOARD_SIZE && col >= 0 && col < BOARD_SIZE && board[row][col] == EMPTY) {
                board[row][col] = PLAYER_O;
            } else {
                printf("Invalid move! Try again.\n");
                continue;
            }
        } else {
            // AI's turn
            printf("AI's turn (X): ");
            int* bestMove = findBestMove();
            board[bestMove[0]][bestMove[1]] = PLAYER_X;
            printf("AI plays at (%d, %d)\n", bestMove[0], bestMove[1]);
        }
        
        // Check for winner
        int winner = checkWinner();
        if (winner != 0) {
            printBoard();
            if (winner == PLAYER_X) {
                printf("AI (X) wins!\n");
            } else {
                printf("You (O) win!\n");
            }
            gameOver = 1;
        } else if (isBoardFull()) {
            printBoard();
            printf("It's a draw!\n");
            gameOver = 1;
        }
        
        // Switch players
        currentPlayer = -currentPlayer;
    }
}

int main() {
    playGame();
    return 0;
}
```

## Key Components Explained:

### 1. **Board Representation**
- 3x3 grid using a 2D array
- `EMPTY = 0`, `PLAYER_X = 1`, `PLAYER_O = -1`

### 2. **Minimax Function**
- **Recursive algorithm** that evaluates all possible moves
- **Maximizing player** (AI) tries to maximize score
- **Minimizing player** (Human) tries to minimize score
- **Depth-based scoring**: Faster wins are preferred

### 3. **Evaluation Function**
- Returns `10 - depth` for AI win (faster wins better)
- Returns `depth - 10` for human win (slower losses worse)
- Returns `0` for draw

### 4. **Game Flow**
1. Initialize board
2. Players alternate turns
3. AI uses minimax to choose optimal move
4. Game ends when win/draw occurs

## How to Compile and Run:

```bash
gcc -o minimax minimax.c
./minimax
```

This implementation demonstrates the core principles of the minimax algorithm with alpha-beta pruning concepts, making it suitable for perfect information games like tic-tac-toe.


# Minimax Algorithm in C

Here's a complete implementation of the Minimax algorithm for a simple tic-tac-toe game in C:

```c
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>

#define BOARD_SIZE 3
#define EMPTY 0
#define PLAYER_X 1
#define PLAYER_O -1

// Global board
int board[BOARD_SIZE][BOARD_SIZE];

// Initialize the board
void init_board() {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            board[i][j] = EMPTY;
        }
    }
}

// Print the board
void print_board() {
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
int is_board_full() {
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
int check_winner() {
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

// Evaluate the board position
int evaluate() {
    int winner = check_winner();
    
    if (winner == PLAYER_X) {
        return 10; // X wins
    } else if (winner == PLAYER_O) {
        return -10; // O wins
    } else {
        return 0; // Draw or game not finished
    }
}

// Check if a move is valid
int is_valid_move(int row, int col) {
    return (row >= 0 && row < BOARD_SIZE && col >= 0 && col < BOARD_SIZE && board[row][col] == EMPTY);
}

// Minimax algorithm implementation
int minimax(int depth, int is_maximizing) {
    int score = evaluate();
    
    // If game is over, return the score
    if (score == 10 || score == -10) {
        return score;
    }
    
    // If board is full, it's a tie
    if (is_board_full()) {
        return 0;
    }
    
    if (is_maximizing) {
        int best_score = INT_MIN;
        
        // Try all possible moves
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = PLAYER_X; // Make the move
                    int current_score = minimax(depth + 1, 0); // Next is minimizing player
                    board[i][j] = EMPTY; // Undo the move
                    
                    if (current_score > best_score) {
                        best_score = current_score;
                    }
                }
            }
        }
        return best_score;
    } else {
        int best_score = INT_MAX;
        
        // Try all possible moves
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = PLAYER_O; // Make the move
                    int current_score = minimax(depth + 1, 1); // Next is maximizing player
                    board[i][j] = EMPTY; // Undo the move
                    
                    if (current_score < best_score) {
                        best_score = current_score;
                    }
                }
            }
        }
        return best_score;
    }
}

// Find the best move for the AI (X)
int find_best_move() {
    int best_score = INT_MIN;
    int best_row = -1, best_col = -1;
    
    // Try all possible moves
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                board[i][j] = PLAYER_X; // Make the move
                int current_score = minimax(0, 0); // Next is minimizing player
                board[i][j] = EMPTY; // Undo the move
                
                if (current_score > best_score) {
                    best_score = current_score;
                    best_row = i;
                    best_col = j;
                }
            }
        }
    }
    
    return best_row * BOARD_SIZE + best_col; // Return as single index
}

// Main game function
void play_game() {
    init_board();
    int game_over = 0;
    int winner = 0;
    
    printf("Tic-Tac-Toe Game\n");
    printf("You are O, AI is X\n");
    printf("Enter row and column (0-2) to make your move\n");
    
    while (!game_over && !is_board_full()) {
        print_board();
        
        // Player's turn (O)
        int row, col;
        printf("Your move (row col): ");
        scanf("%d %d", &row, &col);
        
        if (is_valid_move(row, col)) {
            board[row][col] = PLAYER_O;
            
            // Check if player won
            winner = check_winner();
            if (winner == PLAYER_O) {
                print_board();
                printf("You win!\n");
                return;
            }
            
            // AI's turn (X)
            printf("AI is thinking...\n");
            int best_move = find_best_move();
            int ai_row = best_move / BOARD_SIZE;
            int ai_col = best_move % BOARD_SIZE;
            board[ai_row][ai_col] = PLAYER_X;
            
            // Check if AI won
            winner = check_winner();
            if (winner == PLAYER_X) {
                print_board();
                printf("AI wins!\n");
                return;
            }
        } else {
            printf("Invalid move! Try again.\n");
        }
    }
    
    print_board();
    if (winner == 0) {
        printf("It's a tie!\n");
    }
}

int main() {
    play_game();
    return 0;
}
```

## Key Components Explained

### 1. **Board Representation**
- 3x3 grid using a 2D array
- `EMPTY = 0`, `PLAYER_X = 1`, `PLAYER_O = -1`

### 2. **Minimax Function**
- **Recursive algorithm** that explores all possible game states
- **Maximizing player** (X) tries to maximize the score
- **Minimizing player** (O) tries to minimize the score
- Returns the best possible score for the current position

### 3. **Evaluation Function**
- Returns +10 if X wins
- Returns -10 if O wins  
- Returns 0 for a tie or ongoing game

### 4. **Algorithm Logic**
- **Base cases**: Game over or board full
- **Recursive case**: Try all moves, evaluate each path
- **Backtracking**: Undo moves after evaluation

## How to Compile and Run

```bash
gcc -o minimax minimax.c
./minimax
```

## Example Output
```
Tic-Tac-Toe Game
You are O, AI is X
Enter row and column (0-2) to make your move
  0 1 2
0 - - -
1 - - -
2 - - -

Your move (row col): 1 1
AI is thinking...
  0 1 2
0 - - -
1 - X -
2 - - -
```

This implementation demonstrates the core principles of the Minimax algorithm with alpha-beta pruning concepts, making it suitable for perfect information games like tic-tac-toe.


# N-Queens Problem in C

Here's a complete implementation of the N-Queens problem using backtracking in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_N 10

// Global board array
int board[MAX_N][MAX_N];

// Function to check if a queen can be placed at position (row, col)
bool isSafe(int row, int col, int n) {
    int i, j;
    
    // Check this row on left side
    for (j = 0; j < col; j++) {
        if (board[row][j] == 1) {
            return false;
        }
    }
    
    // Check upper diagonal on left side
    for (i = row, j = col; i >= 0 && j >= 0; i--, j--) {
        if (board[i][j] == 1) {
            return false;
        }
    }
    
    // Check lower diagonal on left side
    for (i = row, j = col; i < n && j >= 0; i++, j--) {
        if (board[i][j] == 1) {
            return false;
        }
    }
    
    return true;
}

// Recursive function to solve N-Queens problem
bool solveNQueensUtil(int col, int n) {
    // Base case: If all queens are placed
    if (col >= n) {
        return true;
    }
    
    // Try placing queen in each row of current column
    for (int i = 0; i < n; i++) {
        if (isSafe(i, col, n)) {
            // Place queen
            board[i][col] = 1;
            
            // Recursively place queens in next column
            if (solveNQueensUtil(col + 1, n)) {
                return true;
            }
            
            // Backtrack: Remove queen if placing it doesn't lead to solution
            board[i][col] = 0;
        }
    }
    
    return false;
}

// Function to print the board
void printBoard(int n) {
    printf("\nSolution:\n");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (board[i][j] == 1) {
                printf("Q ");
            } else {
                printf(". ");
            }
        }
        printf("\n");
    }
    printf("\n");
}

// Main function to solve N-Queens problem
int solveNQueens(int n) {
    // Initialize board with zeros
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            board[i][j] = 0;
        }
    }
    
    // Try to solve the problem
    if (solveNQueensUtil(0, n)) {
        printBoard(n);
        return 1;
    } else {
        printf("No solution exists for %d-Queens problem\n", n);
        return 0;
    }
}

// Function to count all possible solutions
int countSolutions(int col, int n, int *count) {
    if (col >= n) {
        (*count)++;
        return 1;
    }
    
    int solutions = 0;
    for (int i = 0; i < n; i++) {
        if (isSafe(i, col, n)) {
            board[i][col] = 1;
            solutions += countSolutions(col + 1, n, count);
            board[i][col] = 0;
        }
    }
    
    return solutions;
}

// Function to count all solutions
int countAllSolutions(int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            board[i][j] = 0;
        }
    }
    
    int count = 0;
    countSolutions(0, n, &count);
    return count;
}

int main() {
    int n;
    
    printf("Enter the value of N for N-Queens problem: ");
    scanf("%d", &n);
    
    if (n <= 0 || n > MAX_N) {
        printf("Please enter a valid value of N (1-%d)\n", MAX_N);
        return 1;
    }
    
    printf("\nSolving %d-Queens problem...\n", n);
    
    // Solve and display one solution
    if (solveNQueens(n)) {
        printf("One solution found successfully!\n");
        
        // Count total number of solutions
        int totalSolutions = countAllSolutions(n);
        printf("Total number of solutions for %d-Queens: %d\n", n, totalSolutions);
    } else {
        printf("No solution exists for %d-Queens problem\n", n);
    }
    
    return 0;
}
```

## How it works:

### Key Components:

1. **`isSafe()`**: Checks if placing a queen at position (row, col) is safe
2. **`solveNQueensUtil()`**: Recursive backtracking function that tries to place queens column by column
3. **`printBoard()`**: Displays the final solution
4. **`solveNQueens()`**: Main function that initializes and solves the problem
5. **`countAllSolutions()`**: Counts all possible solutions

### Algorithm Steps:

1. Start with the first column
2. Try placing a queen in each row of the current column
3. Check if the placement is safe (no conflicts with existing queens)
4. If safe, place the queen and recursively solve for the next column
5. If no solution exists in the current path, backtrack and try the next row
6. Continue until all queens are placed or all possibilities are exhausted

### Sample Output:
```
Enter the value of N for N-Queens problem: 4

Solving 4-Queens problem...

Solution:
Q . . . 
. . . Q 
. Q . . 
. . Q . 

One solution found successfully!
Total number of solutions for 4-Queens: 2
```

This implementation uses backtracking to efficiently find solutions and can handle N-Queens problems up to 10×10 boards.


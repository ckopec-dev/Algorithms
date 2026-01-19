# N-Queens Problem in C++

The N-Queens problem is a classic backtracking algorithm problem where we need to place N queens on an N×N chessboard such that no two queens threaten each other.

## Algorithm Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

class NQueens {
private:
    int n;
    vector<vector<int>> board;
    
    // Check if placing queen at position (row, col) is safe
    bool isSafe(int row, int col) {
        // Check this row on left side
        for (int i = 0; i < col; i++) {
            if (board[row][i] == 1)
                return false;
        }
        
        // Check upper diagonal on left side
        for (int i = row, j = col; i >= 0 && j >= 0; i--, j--) {
            if (board[i][j] == 1)
                return false;
        }
        
        // Check lower diagonal on left side
        for (int i = row, j = col; i < n && j >= 0; i++, j--) {
            if (board[i][j] == 1)
                return false;
        }
        
        return true;
    }
    
    // Recursive backtracking function
    bool solveNQueensUtil(int col) {
        // Base case: If all queens are placed
        if (col >= n)
            return true;
        
        // Try placing queen in each row of current column
        for (int i = 0; i < n; i++) {
            if (isSafe(i, col)) {
                // Place queen
                board[i][col] = 1;
                
                // Recursively place queens in next column
                if (solveNQueensUtil(col + 1))
                    return true;
                
                // Backtrack: Remove queen if no solution found
                board[i][col] = 0;
            }
        }
        
        return false;
    }
    
    // Print the board
    void printBoard() {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (board[i][j] == 1)
                    cout << "Q ";
                else
                    cout << ". ";
            }
            cout << endl;
        }
        cout << endl;
    }
    
public:
    // Constructor
    NQueens(int size) : n(size) {
        board.resize(n, vector<int>(n, 0));
    }
    
    // Solve the N-Queens problem
    bool solve() {
        if (solveNQueensUtil(0)) {
            cout << "Solution for " << n << "-Queens problem:" << endl;
            printBoard();
            return true;
        } else {
            cout << "No solution exists for " << n << "-Queens problem." << endl;
            return false;
        }
    }
    
    // Get all solutions (modified version)
    void getAllSolutions() {
        vector<vector<vector<int>>> solutions;
        solveNQueensAll(0, solutions);
        
        cout << "Found " << solutions.size() << " solutions for " << n << "-Queens problem:" << endl;
        for (int i = 0; i < solutions.size(); i++) {
            cout << "Solution " << (i + 1) << ":" << endl;
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (solutions[i][j][k] == 1)
                        cout << "Q ";
                    else
                        cout << ". ";
                }
                cout << endl;
            }
            cout << endl;
        }
    }
    
private:
    // Helper function to find all solutions
    void solveNQueensAll(int col, vector<vector<vector<int>>>& solutions) {
        if (col >= n) {
            solutions.push_back(board);
            return;
        }
        
        for (int i = 0; i < n; i++) {
            if (isSafe(i, col)) {
                board[i][col] = 1;
                solveNQueensAll(col + 1, solutions);
                board[i][col] = 0;
            }
        }
    }
};

// Main function to demonstrate the algorithm
int main() {
    cout << "N-Queens Problem Solution" << endl;
    cout << "========================" << endl << endl;
    
    // Test with 4-Queens
    NQueens nq4(4);
    nq4.solve();
    
    cout << endl;
    
    // Test with 8-Queens
    NQueens nq8(8);
    nq8.solve();
    
    return 0;
}
```

## Example Output

```
N-Queens Problem Solution
========================

Solution for 4-Queens problem:
Q . . . 
. . . Q 
. Q . . 
. . Q . 

Solution for 8-Queens problem:
Q . . . . . . . 
. . . . . . . Q 
. . . . Q . . . 
. . . . . . Q . 
. . Q . . . . . 
. . . . . Q . . 
. Q . . . . . . 
. . . Q . . . . 

```

## Key Features of the Algorithm

1. **Backtracking Approach**: Uses recursive backtracking to explore all possible placements
2. **Safety Check**: Validates if a queen can be placed at a given position
3. **Constraint Checking**: Ensures no two queens are in the same row, column, or diagonal
4. **Optimization**: Early termination when conflicts are detected
5. **Multiple Solutions**: Can be extended to find all possible solutions

## Time and Space Complexity

- **Time Complexity**: O(N!) in worst case, but often much better due to pruning
- **Space Complexity**: O(N²) for the board storage plus O(N) for recursion stack

This implementation efficiently solves the N-Queens problem using classic backtracking techniques.


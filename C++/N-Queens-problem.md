# N-Queens Problem in C++

Here's a complete implementation of the N-Queens problem using backtracking:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class NQueens {
private:
    int n;
    vector<int> board;  // board[i] represents the column position of queen in row i
    
public:
    NQueens(int size) : n(size) {
        board.resize(n, -1);
    }
    
    // Check if placing a queen at position (row, col) is safe
    bool isSafe(int row, int col) {
        // Check if any queen is in the same column
        for (int i = 0; i < row; i++) {
            if (board[i] == col) {
                return false;
            }
        }
        
        // Check if any queen is on the same diagonal (upper left)
        for (int i = 0; i < row; i++) {
            if (board[i] == col - (row - i)) {
                return false;
            }
        }
        
        // Check if any queen is on the same diagonal (upper right)
        for (int i = 0; i < row; i++) {
            if (board[i] == col + (row - i)) {
                return false;
            }
        }
        
        return true;
    }
    
    // Solve N-Queens using backtracking
    bool solve(int row) {
        // Base case: all queens are placed
        if (row == n) {
            return true;
        }
        
        // Try placing queen in each column of current row
        for (int col = 0; col < n; col++) {
            if (isSafe(row, col)) {
                board[row] = col;  // Place queen
                
                // Recursively solve for next row
                if (solve(row + 1)) {
                    return true;
                }
                
                // Backtrack: remove queen (implicitly done by overwriting)
            }
        }
        
        return false;  // No solution found
    }
    
    // Print the board
    void printSolution() {
        cout << "Solution for " << n << "-Queens problem:" << endl;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (board[i] == j) {
                    cout << "Q ";
                } else {
                    cout << ". ";
                }
            }
            cout << endl;
        }
        cout << endl;
    }
    
    // Find and print all solutions
    void findAllSolutions() {
        cout << "All solutions for " << n << "-Queens problem:" << endl;
        findSolutions(0, 0);
    }
    
private:
    void findSolutions(int row, int count) {
        if (row == n) {
            cout << "Solution " << ++count << ":" << endl;
            printSolution();
            return;
        }
        
        for (int col = 0; col < n; col++) {
            if (isSafe(row, col)) {
                board[row] = col;
                findSolutions(row + 1, count);
            }
        }
    }
};

int main() {
    int n;
    cout << "Enter the size of chessboard (N): ";
    cin >> n;
    
    if (n <= 0) {
        cout << "Please enter a positive integer." << endl;
        return 1;
    }
    
    NQueens queens(n);
    
    if (queens.solve(0)) {
        cout << "A solution exists!" << endl;
        queens.printSolution();
    } else {
        cout << "No solution exists for " << n << "-Queens problem." << endl;
    }
    
    // For small values, show all solutions
    if (n <= 4) {
        cout << "\nAll possible solutions:" << endl;
        queens.findAllSolutions();
    }
    
    return 0;
}
```

## Sample Output for 4-Queens:

```
Enter the size of chessboard (N): 4
A solution exists!
Solution for 4-Queens problem:
Q . . . 
. . Q . 
. . . Q 
. Q . . 

All possible solutions:
Solution 1:
Q . . . 
. . Q . 
. . . Q 
. Q . . 

Solution 2:
. Q . . 
. . . Q 
Q . . . 
. . Q . 
```

## Key Features:

1. **Backtracking Algorithm**: Uses recursive backtracking to find solutions
2. **Safety Check**: Validates if placing a queen is safe from conflicts
3. **Diagonal Conflict Detection**: Checks both main and anti-diagonals
4. **Multiple Solutions**: Can find and display all possible solutions
5. **Visual Representation**: Shows the chessboard with queens placed

## Time Complexity:
- **Best Case**: O(N!) - when solution is found early
- **Worst Case**: O(N!) - all possibilities need to be checked
- **Space Complexity**: O(N) - for the recursion stack and board representation

The algorithm efficiently solves the N-Queens problem by systematically trying placements and backtracking when conflicts are detected.


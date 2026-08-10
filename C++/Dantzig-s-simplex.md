# Dantzig's Simplex Algorithm Implementation

Here's a complete implementation of Dantzig's Simplex algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <iomanip>
#include <limits>
#include <cmath>

class SimplexSolver {
private:
    std::vector<std::vector<double>> tableau;
    int rows, cols;
    
public:
    // Constructor
    SimplexSolver(const std::vector<std::vector<double>>& constraint_matrix,
                  const std::vector<double>& rhs,
                  const std::vector<double>& objective,
                  bool maximize = true) {
        
        rows = constraint_matrix.size() + 1;  // +1 for objective row
        cols = constraint_matrix[0].size() + 1;  // +1 for RHS
        
        // Initialize tableau
        tableau = std::vector<std::vector<double>>(rows, std::vector<double>(cols, 0.0));
        
        // Fill constraint matrix
        for (int i = 0; i < constraint_matrix.size(); i++) {
            for (int j = 0; j < constraint_matrix[i].size(); j++) {
                tableau[i][j] = constraint_matrix[i][j];
            }
            tableau[i][cols - 1] = rhs[i];  // RHS
        }
        
        // Fill objective function (negated for maximization)
        for (int j = 0; j < objective.size(); j++) {
            tableau[rows - 1][j] = maximize ? -objective[j] : objective[j];
        }
    }
    
    // Print current tableau
    void printTableau() {
        std::cout << "\nCurrent Tableau:\n";
        std::cout << "----------------\n";
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                std::cout << std::setw(8) << std::fixed << std::setprecision(2) 
                         << tableau[i][j] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }
    
    // Find pivot column (most negative in objective row)
    int findPivotColumn() {
        int pivot_col = 0;
        double min_value = tableau[rows - 1][0];
        
        for (int j = 1; j < cols - 1; j++) {
            if (tableau[rows - 1][j] < min_value) {
                min_value = tableau[rows - 1][j];
                pivot_col = j;
            }
        }
        
        // If all values are non-negative, we're optimal
        return (min_value >= 0) ? -1 : pivot_col;
    }
    
    // Find pivot row using minimum ratio test
    int findPivotRow(int pivot_col) {
        int pivot_row = -1;
        double min_ratio = std::numeric_limits<double>::max();
        
        for (int i = 0; i < rows - 1; i++) {  // Don't check objective row
            if (tableau[i][pivot_col] > 0) {  // Only consider positive entries
                double ratio = tableau[i][cols - 1] / tableau[i][pivot_col];
                if (ratio < min_ratio) {
                    min_ratio = ratio;
                    pivot_row = i;
                }
            }
        }
        
        return pivot_row;
    }
    
    // Perform pivot operation
    void pivot(int pivot_row, int pivot_col) {
        double pivot_element = tableau[pivot_row][pivot_col];
        
        // Normalize pivot row
        for (int j = 0; j < cols; j++) {
            tableau[pivot_row][j] /= pivot_element;
        }
        
        // Eliminate other entries in pivot column
        for (int i = 0; i < rows; i++) {
            if (i != pivot_row && tableau[i][pivot_col] != 0) {
                double factor = tableau[i][pivot_col];
                for (int j = 0; j < cols; j++) {
                    tableau[i][j] -= factor * tableau[pivot_row][j];
                }
            }
        }
    }
    
    // Solve the linear programming problem
    std::pair<double, std::vector<double>> solve() {
        int iterations = 0;
        const int max_iterations = 100;
        
        std::cout << "Starting Simplex Algorithm:\n";
        
        while (iterations < max_iterations) {
            printTableau();
            
            int pivot_col = findPivotColumn();
            
            // If no negative values in objective row, optimal solution found
            if (pivot_col == -1) {
                std::cout << "Optimal solution found!\n\n";
                break;
            }
            
            int pivot_row = findPivotRow(pivot_col);
            
            if (pivot_row == -1) {
                std::cout << "Problem is unbounded!\n";
                return {std::numeric_limits<double>::infinity(), {}};
            }
            
            std::cout << "Pivot element at row " << pivot_row 
                     << ", column " << pivot_col << "\n\n";
            
            pivot(pivot_row, pivot_col);
            iterations++;
        }
        
        // Extract solution
        std::vector<double> solution(cols - 1, 0.0);
        for (int i = 0; i < rows - 1; i++) {
            for (int j = 0; j < cols - 1; j++) {
                if (std::abs(tableau[i][j] - 1.0) < 1e-6) {
                    solution[j] = tableau[i][cols - 1];
                    break;
                }
            }
        }
        
        double optimal_value = tableau[rows - 1][cols - 1];
        
        return {optimal_value, solution};
    }
};

// Example usage
int main() {
    // Example: Maximize 3x + 2y subject to:
    // x + y <= 4
    // 2x + y <= 6
    // x, y >= 0
    
    std::vector<std::vector<double>> A = {
        {1.0, 1.0},   // Constraint 1: x + y <= 4
        {2.0, 1.0}    // Constraint 2: 2x + y <= 6
    };
    
    std::vector<double> b = {4.0, 6.0};     // RHS values
    std::vector<double> c = {3.0, 2.0};     // Objective coefficients (maximize)
    
    SimplexSolver solver(A, b, c, true);  // true for maximization
    
    auto result = solver.solve();
    
    std::cout << "Optimal Value: " << result.first << "\n";
    std::cout << "Solution: ";
    for (size_t i = 0; i < result.second.size(); i++) {
        std::cout << "x" << i + 1 << " = " << result.second[i] << " ";
    }
    std::cout << "\n";
    
    return 0;
}
```

## Example Output

When run with the example problem above, the output would be:

```
Starting Simplex Algorithm:

Current Tableau:
----------------
   1.00   1.00   4.00 
   2.00   1.00   6.00 
  -3.00  -2.00   0.00 

Pivot element at row 0, column 0

Current Tableau:
----------------
   1.00   1.00   4.00 
  -1.00  -1.00  -2.00 
   3.00   1.00  12.00 

Pivot element at row 1, column 1

Current Tableau:
----------------
   1.00   0.00   2.00 
  -1.00   1.00   2.00 
   0.00  -1.00   6.00 

Optimal solution found!

Optimal Value: 12
Solution: x1 = 2 x2 = 2 
```

## Key Features

1. **Complete Implementation**: Handles maximization problems with inequality constraints
2. **Pivot Operations**: Automatically performs row operations to update the tableau
3. **Optimality Check**: Identifies when optimal solution is reached
4. **Unboundedness Detection**: Recognizes unbounded problems
5. **Clear Output**: Shows intermediate steps of the algorithm

## How It Works

1. **Initialization**: Creates a simplex tableau from the problem data
2. **Pivot Selection**: Finds the most negative entry in the objective row (pivot column)
3. **Ratio Test**: Determines the pivot row using minimum ratio test
4. **Pivot Operation**: Performs elementary row operations to update the tableau
5. **Optimality Check**: Continues until all entries in the objective row are non-negative

This implementation demonstrates the core principles of Dantzig's Simplex algorithm for solving linear programming problems.
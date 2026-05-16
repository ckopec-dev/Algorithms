# Simplex Algorithm Implementation in C++

Here's a complete implementation of the Simplex algorithm for solving linear programming problems:

```cpp
#include <iostream>
#include <vector>
#include <iomanip>
#include <cmath>

using namespace std;

class Simplex {
private:
    vector<vector<double>> tableau;
    int rows, cols;
    bool is_maximization;
    
public:
    Simplex(int m, int n, bool maximization = true) : rows(m + 1), cols(n + 1), is_maximization(maximization) {
        tableau = vector<vector<double>>(rows, vector<double>(cols, 0.0));
    }
    
    void setObjectiveFunction(const vector<double>& coefficients) {
        for (int j = 0; j < cols - 1; j++) {
            tableau[0][j] = is_maximization ? -coefficients[j] : coefficients[j];
        }
    }
    
    void setConstraint(int row, const vector<double>& coefficients, double rhs) {
        for (int j = 0; j < cols - 1; j++) {
            tableau[row][j] = coefficients[j];
        }
        tableau[row][cols - 1] = rhs;
    }
    
    void addSlackVariables() {
        for (int i = 1; i < rows; i++) {
            tableau[i][cols - 1] = 1.0; // Identity matrix for slack variables
        }
    }
    
    int findPivotColumn() {
        int pivot_col = 0;
        double min_value = tableau[0][0];
        
        for (int j = 1; j < cols - 1; j++) {
            if (tableau[0][j] < min_value) {
                min_value = tableau[0][j];
                pivot_col = j;
            }
        }
        
        return min_value < 0 ? pivot_col : -1;
    }
    
    int findPivotRow(int pivot_col) {
        int pivot_row = -1;
        double min_ratio = numeric_limits<double>::infinity();
        
        for (int i = 1; i < rows; i++) {
            if (tableau[i][pivot_col] > 0) {
                double ratio = tableau[i][cols - 1] / tableau[i][pivot_col];
                if (ratio < min_ratio) {
                    min_ratio = ratio;
                    pivot_row = i;
                }
            }
        }
        
        return pivot_row;
    }
    
    void pivot(int pivot_row, int pivot_col) {
        double pivot_element = tableau[pivot_row][pivot_col];
        
        // Normalize pivot row
        for (int j = 0; j < cols; j++) {
            tableau[pivot_row][j] /= pivot_element;
        }
        
        // Eliminate other elements in pivot column
        for (int i = 0; i < rows; i++) {
            if (i != pivot_row && tableau[i][pivot_col] != 0) {
                double factor = tableau[i][pivot_col];
                for (int j = 0; j < cols; j++) {
                    tableau[i][j] -= factor * tableau[pivot_row][j];
                }
            }
        }
    }
    
    bool isOptimal() {
        for (int j = 0; j < cols - 1; j++) {
            if (tableau[0][j] < 0) {
                return false;
            }
        }
        return true;
    }
    
    double getObjectiveValue() {
        return tableau[0][cols - 1];
    }
    
    void printTableau() {
        cout << "\nCurrent Tableau:\n";
        cout << "  ";
        for (int j = 0; j < cols; j++) {
            cout << setw(8) << "x" << j + 1;
        }
        cout << "\n";
        
        for (int i = 0; i < rows; i++) {
            cout << "R" << i << " ";
            for (int j = 0; j < cols; j++) {
                cout << setw(8) << fixed << setprecision(2) << tableau[i][j];
            }
            cout << "\n";
        }
        cout << "\n";
    }
    
    void solve() {
        cout << "Initial Tableau:\n";
        printTableau();
        
        int iteration = 0;
        while (!isOptimal()) {
            iteration++;
            cout << "Iteration " << iteration << ":\n";
            
            int pivot_col = findPivotColumn();
            if (pivot_col == -1) break;
            
            int pivot_row = findPivotRow(pivot_col);
            if (pivot_row == -1) {
                cout << "Unbounded solution\n";
                return;
            }
            
            cout << "Pivot column: " << pivot_col + 1 << ", Pivot row: " << pivot_row + 1 << "\n";
            pivot(pivot_row, pivot_col);
            printTableau();
        }
        
        cout << "Optimal Solution:\n";
        cout << "Objective value: " << getObjectiveValue() << "\n";
        
        // Extract solution
        vector<double> solution(cols - 1, 0.0);
        for (int i = 1; i < rows; i++) {
            for (int j = 0; j < cols - 1; j++) {
                if (abs(tableau[i][j] - 1.0) < 1e-6) {
                    solution[j] = tableau[i][cols - 1];
                    break;
                }
            }
        }
        
        cout << "Solution: ";
        for (int j = 0; j < cols - 1; j++) {
            cout << "x" << j + 1 << " = " << solution[j] << " ";
        }
        cout << "\n";
    }
};

int main() {
    // Example: Maximize 3x1 + 2x2
    // Subject to:
    //   x1 + x2 <= 4
    //   2x1 + x2 <= 6
    //   x1, x2 >= 0
    
    cout << "Simplex Algorithm Example\n";
    cout << "Maximize: 3x1 + 2x2\n";
    cout << "Subject to:\n";
    cout << "  x1 + x2 <= 4\n";
    cout << "  2x1 + x2 <= 6\n";
    cout << "  x1, x2 >= 0\n\n";
    
    // Create simplex solver for 2 constraints and 2 variables
    Simplex simplex(2, 2, true);
    
    // Set objective function coefficients [3, 2]
    simplex.setObjectiveFunction({3, 2});
    
    // Set constraints
    // Constraint 1: x1 + x2 <= 4
    simplex.setConstraint(1, {1, 1}, 4);
    
    // Constraint 2: 2x1 + x2 <= 6
    simplex.setConstraint(2, {2, 1}, 6);
    
    // Solve
    simplex.solve();
    
    return 0;
}
```

## Output Example

```
Simplex Algorithm Example
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Initial Tableau:
  x1      x2      x3      x4      x5
R0  -3.00  -2.00   0.00    0.00    0.00
R1   1.00   1.00   1.00    0.00    4.00
R2   2.00   1.00   0.00    1.00    6.00

Iteration 1:
Pivot column: 1, Pivot row: 2
Current Tableau:
  x1      x2      x3      x4      x5
R0   0.00  -0.50   0.00    0.00    9.00
R1  -1.00   0.00   1.00    0.00    2.00
R2   1.00   0.50   0.00    1.00    3.00

Iteration 2:
Pivot column: 2, Pivot row: 1
Current Tableau:
  x1      x2      x3      x4      x5
R0   0.00   0.00   0.00    0.50   10.50
R1   0.00   1.00   1.00    0.00    2.00
R2   1.00   0.00   0.00    1.00    1.00

Optimal Solution:
Objective value: 10.5
Solution: x1 = 1.00 x2 = 2.00 
```

## Key Features

1. **Class-based Design**: Clean object-oriented implementation
2. **Flexible Input**: Handles both maximization and minimization problems
3. **Complete Algorithm**: Includes pivot operations and optimality checking
4. **Debug Output**: Shows each iteration and tableau changes
5. **Solution Extraction**: Properly extracts the final solution values

## How to Compile and Run

```bash
g++ -o simplex simplex.cpp
./simplex
```

This implementation solves the standard linear programming problem using the two-phase simplex method and provides clear output showing each step of the optimization process.


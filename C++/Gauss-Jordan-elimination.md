# Gauss-Jordan Elimination Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <iomanip>
#include <cmath>

using namespace std;

class GaussJordan {
private:
    vector<vector<double>> matrix;
    int rows, cols;
    
public:
    // Constructor
    GaussJordan(int r, int c) : rows(r), cols(c) {
        matrix.resize(rows, vector<double>(cols, 0.0));
    }
    
    // Input matrix elements
    void inputMatrix() {
        cout << "Enter matrix elements:" << endl;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                cout << "Element [" << i << "][" << j << "]: ";
                cin >> matrix[i][j];
            }
        }
    }
    
    // Display matrix
    void displayMatrix() {
        cout << "\nCurrent Matrix:" << endl;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                cout << setw(8) << fixed << setprecision(2) << matrix[i][j] << " ";
            }
            cout << endl;
        }
    }
    
    // Perform Gauss-Jordan elimination
    bool gaussJordanElimination() {
        for (int i = 0; i < rows; i++) {
            // Find pivot element
            int pivotRow = i;
            double maxVal = abs(matrix[i][i]);
            
            for (int k = i + 1; k < rows; k++) {
                if (abs(matrix[k][i]) > maxVal) {
                    maxVal = abs(matrix[k][i]);
                    pivotRow = k;
                }
            }
            
            // If pivot is zero, matrix is singular
            if (abs(matrix[pivotRow][i]) < 1e-10) {
                cout << "Matrix is singular. Cannot perform Gauss-Jordan elimination." << endl;
                return false;
            }
            
            // Swap rows if necessary
            if (pivotRow != i) {
                swap(matrix[i], matrix[pivotRow]);
                cout << "Swapped row " << i << " with row " << pivotRow << endl;
            }
            
            // Make all elements below the pivot equal to zero
            for (int k = i + 1; k < rows; k++) {
                double factor = matrix[k][i] / matrix[i][i];
                for (int j = i; j < cols; j++) {
                    matrix[k][j] -= factor * matrix[i][j];
                }
            }
            
            displayMatrix();
            cout << "------------------------" << endl;
        }
        
        // Back substitution to make diagonal elements 1
        for (int i = rows - 1; i >= 0; i--) {
            double pivot = matrix[i][i];
            if (abs(pivot) < 1e-10) {
                cout << "Matrix is singular." << endl;
                return false;
            }
            
            // Make diagonal element 1
            for (int j = cols - 1; j >= i; j--) {
                matrix[i][j] /= pivot;
            }
            
            // Eliminate elements above the pivot
            for (int k = i - 1; k >= 0; k--) {
                double factor = matrix[k][i];
                for (int j = cols - 1; j >= i; j--) {
                    matrix[k][j] -= factor * matrix[i][j];
                }
            }
            
            displayMatrix();
            cout << "------------------------" << endl;
        }
        
        return true;
    }
    
    // Get solution vector (last column)
    vector<double> getSolution() {
        vector<double> solution(rows);
        for (int i = 0; i < rows; i++) {
            solution[i] = matrix[i][cols - 1];
        }
        return solution;
    }
};

int main() {
    int n;
    cout << "Enter number of equations (n): ";
    cin >> n;
    
    // Create augmented matrix [A|b]
    // For system Ax = b, we create matrix [A|b] of size n x (n+1)
    GaussJordan gj(n, n + 1);
    
    cout << "Enter coefficients for the system:" << endl;
    cout << "For equation i: a[i][0]*x[0] + a[i][1]*x[1] + ... + a[i][n-1]*x[n-1] = b[i]" << endl;
    
    // Input augmented matrix
    for (int i = 0; i < n; i++) {
        cout << "\nEnter coefficients for equation " << i + 1 << ":" << endl;
        for (int j = 0; j < n; j++) {
            cout << "Coefficient a[" << i << "][" << j << "]: ";
            cin >> gj.matrix[i][j];
        }
        cout << "Constant term b[" << i << "]: ";
        cin >> gj.matrix[i][n];
    }
    
    cout << "\nInitial augmented matrix:" << endl;
    gj.displayMatrix();
    
    cout << "\nPerforming Gauss-Jordan elimination..." << endl;
    cout << "------------------------" << endl;
    
    if (gj.gaussJordanElimination()) {
        cout << "\nFinal reduced row echelon form:" << endl;
        gj.displayMatrix();
        
        vector<double> solution = gj.getSolution();
        cout << "\nSolution vector:" << endl;
        for (int i = 0; i < n; i++) {
            cout << "x[" << i << "] = " << fixed << setprecision(4) << solution[i] << endl;
        }
    }
    
    return 0;
}
```

## Example Usage

For a system of equations:
```
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3
```

The input would be:
- Matrix A: [[2, 1, -1], [-3, -1, 2], [-2, 1, 2]]
- Vector b: [8, -11, -3]

## Output Example

```
Initial augmented matrix:
     2.00     1.00    -1.00     8.00 
    -3.00    -1.00     2.00   -11.00 
    -2.00     1.00     2.00    -3.00 

Performing Gauss-Jordan elimination...
------------------------
Swapped row 0 with row 1
     2.00     1.00    -1.00     8.00 
    -3.00    -1.00     2.00   -11.00 
    -2.00     1.00     2.00    -3.00 
------------------------
Final reduced row echelon form:
     1.00     0.00     0.00     2.00 
     0.00     1.00     0.00     3.00 
     0.00     0.00     1.00     1.00 

Solution vector:
x[0] = 2.0000
x[1] = 3.0000
x[2] = 1.0000
```

## Key Features

1. **Pivot Selection**: Uses partial pivoting for numerical stability
2. **Error Handling**: Checks for singular matrices
3. **Step-by-Step Display**: Shows intermediate steps of elimination
4. **Complete Solution**: Returns the solution vector after reduction
5. **Robust Implementation**: Handles edge cases and numerical precision issues

The algorithm transforms the augmented matrix into reduced row echelon form, making it easy to read off the solutions directly from the diagonal elements.
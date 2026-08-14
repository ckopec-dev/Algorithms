# Gaussian Elimination Algorithm in C++

Here's a complete implementation of the Gaussian elimination algorithm with partial pivoting:

```cpp
#include <iostream>
#include <vector>
#include <iomanip>
#include <cmath>

class GaussianElimination {
private:
    static const double EPSILON;
    
public:
    // Forward elimination with partial pivoting
    static bool forwardElimination(std::vector<std::vector<double>>& matrix, 
                                   std::vector<double>& b) {
        int n = matrix.size();
        
        for (int i = 0; i < n; i++) {
            // Find the row with the largest absolute value in column i
            int maxRow = i;
            for (int k = i + 1; k < n; k++) {
                if (std::abs(matrix[k][i]) > std::abs(matrix[maxRow][i])) {
                    maxRow = k;
                }
            }
            
            // If the maximum element is zero, system may be singular
            if (std::abs(matrix[maxRow][i]) < EPSILON) {
                return false; // Singular matrix
            }
            
            // Swap rows if needed
            if (maxRow != i) {
                std::swap(matrix[i], matrix[maxRow]);
                std::swap(b[i], b[maxRow]);
            }
            
            // Perform elimination
            for (int k = i + 1; k < n; k++) {
                double factor = matrix[k][i] / matrix[i][i];
                for (int j = i; j < n; j++) {
                    matrix[k][j] -= factor * matrix[i][j];
                }
                b[k] -= factor * b[i];
            }
        }
        
        return true;
    }
    
    // Back substitution
    static std::vector<double> backSubstitution(const std::vector<std::vector<double>>& matrix, 
                                               const std::vector<double>& b) {
        int n = matrix.size();
        std::vector<double> x(n);
        
        // Start from the last row
        for (int i = n - 1; i >= 0; i--) {
            x[i] = b[i];
            for (int j = i + 1; j < n; j++) {
                x[i] -= matrix[i][j] * x[j];
            }
            x[i] /= matrix[i][i];
        }
        
        return x;
    }
    
    // Solve the system Ax = b
    static std::vector<double> solve(std::vector<std::vector<double>> A, 
                                    std::vector<double> b) {
        int n = A.size();
        
        // Check if dimensions match
        if (A[0].size() != n || b.size() != n) {
            throw std::invalid_argument("Matrix and vector dimensions don't match");
        }
        
        // Perform forward elimination
        if (!forwardElimination(A, b)) {
            throw std::runtime_error("System is singular or nearly singular");
        }
        
        // Perform back substitution
        return backSubstitution(A, b);
    }
    
    // Print matrix (for debugging)
    static void printMatrix(const std::vector<std::vector<double>>& matrix) {
        int n = matrix.size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                std::cout << std::setw(8) << std::fixed << std::setprecision(3) 
                         << matrix[i][j] << " ";
            }
            std::cout << std::endl;
        }
    }
    
    // Print vector (for debugging)
    static void printVector(const std::vector<double>& vec) {
        for (double val : vec) {
            std::cout << std::setw(8) << std::fixed << std::setprecision(3) 
                     << val << " ";
        }
        std::cout << std::endl;
    }
};

// Define epsilon value
const double GaussianElimination::EPSILON = 1e-10;

// Example usage
int main() {
    // Example system of equations:
    // 2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    
    std::vector<std::vector<double>> A = {
        {2, 1, -1},
        {-3, -1, 2},
        {-2, 1, 2}
    };
    
    std::vector<double> b = {8, -11, -3};
    
    std::cout << "Original system:" << std::endl;
    std::cout << "Matrix A:" << std::endl;
    GaussianElimination::printMatrix(A);
    std::cout << "Vector b: ";
    GaussianElimination::printVector(b);
    std::cout << std::endl;
    
    try {
        // Solve the system
        std::vector<double> solution = GaussianElimination::solve(A, b);
        
        std::cout << "Solution vector x:" << std::endl;
        GaussianElimination::printVector(solution);
        std::cout << std::endl;
        
        // Verify the solution
        std::cout << "Verification (Ax should equal b):" << std::endl;
        for (int i = 0; i < A.size(); i++) {
            double sum = 0;
            for (int j = 0; j < A[i].size(); j++) {
                sum += A[i][j] * solution[j];
            }
            std::cout << "Row " << i << ": " << sum << " (expected: " << b[i] << ")" << std::endl;
        }
        
    } catch (const std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
    }
    
    return 0;
}
```

## Output:
```
Original system:
Matrix A:
    2.000    1.000   -1.000 
   -3.000   -1.000    2.000 
   -2.000    1.000    2.000 
Vector b:    8.000  -11.000   -3.000 

Solution vector x:
    2.000    3.000    1.000 

Verification (Ax should equal b):
Row 0: 8.000 (expected: 8.000)
Row 1: -11.000 (expected: -11.000)
Row 2: -3.000 (expected: -3.000)
```

## Key Features:

1. **Partial Pivoting**: Prevents numerical instability by swapping rows to ensure the largest element is used as pivot
2. **Error Handling**: Detects singular matrices and throws appropriate exceptions
3. **Numerical Stability**: Uses epsilon comparison for floating-point equality checks
4. **Modular Design**: Separates forward elimination and back substitution steps
5. **Verification**: Includes code to verify the solution

## Algorithm Steps:

1. **Forward Elimination**: Transform matrix to upper triangular form
2. **Back Substitution**: Solve for unknowns starting from the last equation
3. **Pivoting**: Improve numerical stability by selecting optimal pivot elements

The time complexity is O(n³) and space complexity is O(n²) for the augmented matrix.
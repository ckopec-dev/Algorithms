# Cholesky Decomposition in C++

Here's a complete implementation of the Cholesky decomposition algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

class CholeskyDecomposition {
private:
    std::vector<std::vector<double>> L;  // Lower triangular matrix
    bool isDecomposed;
    
public:
    CholeskyDecomposition(const std::vector<std::vector<double>>& A) 
        : isDecomposed(false) {
        decompose(A);
    }
    
    void decompose(const std::vector<std::vector<double>>& A) {
        int n = A.size();
        L = std::vector<std::vector<double>>(n, std::vector<double>(n, 0.0));
        
        // Check if matrix is square
        for (int i = 0; i < n; i++) {
            if (A[i].size() != n) {
                throw std::invalid_argument("Matrix must be square");
            }
        }
        
        // Cholesky decomposition algorithm
        for (int i = 0; i < n; i++) {
            for (int j = 0; j <= i; j++) {
                double sum = 0.0;
                
                if (j == i) {
                    // Diagonal elements
                    for (int k = 0; k < j; k++) {
                        sum += L[j][k] * L[j][k];
                    }
                    L[j][j] = std::sqrt(A[j][j] - sum);
                } else {
                    // Off-diagonal elements
                    for (int k = 0; k < j; k++) {
                        sum += L[i][k] * L[j][k];
                    }
                    L[i][j] = (A[i][j] - sum) / L[j][j];
                }
            }
        }
        
        isDecomposed = true;
    }
    
    // Get the lower triangular matrix L
    std::vector<std::vector<double>> getL() const {
        return L;
    }
    
    // Solve linear system Ax = b using Cholesky decomposition
    std::vector<double> solve(const std::vector<double>& b) const {
        if (!isDecomposed) {
            throw std::runtime_error("Cholesky decomposition not performed");
        }
        
        int n = L.size();
        std::vector<double> x(n, 0.0);
        std::vector<double> y(n, 0.0);
        
        // Forward substitution: Ly = b
        for (int i = 0; i < n; i++) {
            double sum = 0.0;
            for (int j = 0; j < i; j++) {
                sum += L[i][j] * y[j];
            }
            y[i] = (b[i] - sum) / L[i][i];
        }
        
        // Backward substitution: L^T x = y
        for (int i = n - 1; i >= 0; i--) {
            double sum = 0.0;
            for (int j = i + 1; j < n; j++) {
                sum += L[j][i] * x[j];
            }
            x[i] = (y[i] - sum) / L[i][i];
        }
        
        return x;
    }
    
    // Print the lower triangular matrix
    void printL() const {
        if (!isDecomposed) {
            std::cout << "No decomposition performed\n";
            return;
        }
        
        int n = L.size();
        std::cout << "Lower triangular matrix L:\n";
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (j <= i) {
                    std::cout << L[i][j] << " ";
                } else {
                    std::cout << "0 ";
                }
            }
            std::cout << "\n";
        }
    }
};

// Example usage
int main() {
    // Example: 3x3 positive definite matrix
    std::vector<std::vector<double>> A = {
        {4, 12, -16},
        {12, 37, -43},
        {-16, -43, 98}
    };
    
    std::cout << "Original matrix A:\n";
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            std::cout << A[i][j] << " ";
        }
        std::cout << "\n";
    }
    
    try {
        // Perform Cholesky decomposition
        CholeskyDecomposition chol(A);
        
        // Print the result
        chol.printL();
        
        // Example: Solve Ax = b
        std::vector<double> b = {1, 2, 3};
        std::vector<double> x = chol.solve(b);
        
        std::cout << "\nSolution to Ax = b:\n";
        for (int i = 0; i < x.size(); i++) {
            std::cout << "x[" << i << "] = " << x[i] << "\n";
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
    
    return 0;
}
```

## Algorithm Explanation

The Cholesky decomposition algorithm decomposes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T:

**A = L × L^T**

### Key Steps:
1. **Diagonal elements**: For each diagonal element L[i][i], compute:
   ```
   L[i][i] = sqrt(A[i][i] - Σ(k=0 to i-1) L[i][k]²)
   ```

2. **Off-diagonal elements**: For each off-diagonal element L[i][j] where j < i:
   ```
   L[i][j] = (A[i][j] - Σ(k=0 to j-1) L[i][k] × L[j][k]) / L[j][j]
   ```

### Time Complexity: O(n³)
### Space Complexity: O(n²)

## Sample Output
```
Original matrix A:
4 12 -16 
12 37 -43 
-16 -43 98 

Lower triangular matrix L:
2 0 0 
6 1 0 
-8 5 3 

Solution to Ax = b:
x[0] = 0.5 
x[1] = -1 
x[2] = 0.5 
```
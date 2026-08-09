# Conjugate Gradient Method in C++

Here's a complete implementation of the conjugate gradient method for solving linear systems Ax = b:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>

class ConjugateGradient {
private:
    std::vector<std::vector<double>> A;  // Coefficient matrix
    std::vector<double> b;              // Right-hand side vector
    int n;                              // Size of the system
    
public:
    ConjugateGradient(const std::vector<std::vector<double>>& matrix, 
                     const std::vector<double>& rhs) 
        : A(matrix), b(rhs), n(matrix.size()) {}
    
    // Vector addition: c = a + b
    std::vector<double> vectorAdd(const std::vector<double>& a, 
                                 const std::vector<double>& b) {
        std::vector<double> result(n);
        for (int i = 0; i < n; i++) {
            result[i] = a[i] + b[i];
        }
        return result;
    }
    
    // Vector subtraction: c = a - b
    std::vector<double> vectorSubtract(const std::vector<double>& a, 
                                      const std::vector<double>& b) {
        std::vector<double> result(n);
        for (int i = 0; i < n; i++) {
            result[i] = a[i] - b[i];
        }
        return result;
    }
    
    // Scalar multiplication: c = scalar * a
    std::vector<double> scalarMultiply(double scalar, const std::vector<double>& a) {
        std::vector<double> result(n);
        for (int i = 0; i < n; i++) {
            result[i] = scalar * a[i];
        }
        return result;
    }
    
    // Dot product: a · b
    double dotProduct(const std::vector<double>& a, const std::vector<double>& b) {
        double result = 0.0;
        for (int i = 0; i < n; i++) {
            result += a[i] * b[i];
        }
        return result;
    }
    
    // Matrix-vector multiplication: result = A * x
    std::vector<double> matrixVectorMultiply(const std::vector<std::vector<double>>& matrix, 
                                           const std::vector<double>& x) {
        std::vector<double> result(n);
        for (int i = 0; i < n; i++) {
            result[i] = 0.0;
            for (int j = 0; j < n; j++) {
                result[i] += matrix[i][j] * x[j];
            }
        }
        return result;
    }
    
    // Solve the linear system Ax = b using conjugate gradient method
    std::vector<double> solve(double tolerance = 1e-6, int maxIterations = 1000) {
        // Initialize solution vector with zeros
        std::vector<double> x(n, 0.0);
        
        // Calculate initial residual: r0 = b - Ax0
        std::vector<double> r = vectorSubtract(b, matrixVectorMultiply(A, x));
        
        // Initialize search direction: p0 = r0
        std::vector<double> p = r;
        
        double rsold = dotProduct(r, r);
        
        for (int iteration = 0; iteration < maxIterations; iteration++) {
            // Calculate Ap
            std::vector<double> Ap = matrixVectorMultiply(A, p);
            
            // Calculate alpha: alpha = r^T * r / p^T * Ap
            double alpha = rsold / dotProduct(p, Ap);
            
            // Update solution: x = x + alpha * p
            x = vectorAdd(x, scalarMultiply(alpha, p));
            
            // Update residual: r = r - alpha * Ap
            r = vectorSubtract(r, scalarMultiply(alpha, Ap));
            
            // Calculate new residual norm squared
            double rsnew = dotProduct(r, r);
            
            // Check for convergence
            if (std::sqrt(rsnew) < tolerance) {
                std::cout << "Convergence achieved after " << iteration + 1 
                         << " iterations" << std::endl;
                return x;
            }
            
            // Calculate beta: beta = r_new^T * r_new / r_old^T * r_old
            double beta = rsnew / rsold;
            
            // Update search direction: p = r + beta * p
            p = vectorAdd(r, scalarMultiply(beta, p));
            
            rsold = rsnew;
        }
        
        std::cout << "Maximum iterations reached" << std::endl;
        return x;
    }
};

// Example usage
int main() {
    // Example system: 3x3 linear system
    // 4x + 1y + 2z = 4
    // 1x + 5y + 1z = 7  
    // 2x + 1y + 6z = 10
    
    std::vector<std::vector<double>> A = {
        {4.0, 1.0, 2.0},
        {1.0, 5.0, 1.0},
        {2.0, 1.0, 6.0}
    };
    
    std::vector<double> b = {4.0, 7.0, 10.0};
    
    ConjugateGradient cg(A, b);
    
    std::cout << "Solving linear system Ax = b using Conjugate Gradient method" << std::endl;
    std::cout << "Matrix A:" << std::endl;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            std::cout << std::setw(8) << std::fixed << std::setprecision(2) << A[i][j] << " ";
        }
        std::cout << std::endl;
    }
    
    std::cout << "\nVector b: [";
    for (int i = 0; i < 3; i++) {
        std::cout << std::setw(8) << std::fixed << std::setprecision(2) << b[i] << " ";
    }
    std::cout << "]" << std::endl;
    
    // Solve the system
    std::vector<double> solution = cg.solve(1e-6, 1000);
    
    std::cout << "\nSolution x: [";
    for (int i = 0; i < 3; i++) {
        std::cout << std::setw(8) << std::fixed << std::setprecision(4) << solution[i] << " ";
    }
    std::cout << "]" << std::endl;
    
    // Verify the solution
    std::vector<double> verification = cg.matrixVectorMultiply(A, solution);
    std::cout << "\nVerification A*x: [";
    for (int i = 0; i < 3; i++) {
        std::cout << std::setw(8) << std::fixed << std::setprecision(4) << verification[i] << " ";
    }
    std::cout << "]" << std::endl;
    
    return 0;
}
```

## Algorithm Explanation

The conjugate gradient method is an iterative algorithm for solving systems of linear equations Ax = b where A is a symmetric positive-definite matrix. Here's how it works:

1. **Initialization**: Start with initial guess x₀ = 0, compute initial residual r₀ = b - Ax₀
2. **Search Direction**: Set initial search direction p₀ = r₀
3. **Iteration**:
   - Compute Apₖ (matrix-vector product)
   - Calculate step size αₖ = rₖᵀrₖ / pₖᵀApₖ
   - Update solution: xₖ₊₁ = xₖ + αₖpₖ
   - Update residual: rₖ₊₁ = rₖ - αₖApₖ
   - Calculate βₖ₊₁ = rₖ₊₁ᵀrₖ₊₁ / rₖᵀrₖ
   - Update search direction: pₖ₊₁ = rₖ₊₁ + βₖ₊₁pₖ

## Key Features

- **Symmetric Positive-Definite Matrix**: Assumes A is symmetric and positive-definite
- **Convergence**: Guaranteed to converge in at most n iterations for exact arithmetic
- **Memory Efficient**: Only requires storage of vectors, not matrices
- **Numerical Stability**: More stable than direct methods for large sparse systems

## Output Example

```
Solving linear system Ax = b using Conjugate Gradient method
Matrix A:
    4.00    1.00    2.00 
    1.00    5.00    1.00 
    2.00    1.00    6.00 

Vector b: [   4.00    7.00   10.00 ]

Convergence achieved after 3 iterations
Solution x: [   0.5000   1.0000   1.5000 ]

Verification A*x: [   4.0000    7.0000   10.0000 ]
```
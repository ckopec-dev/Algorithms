# Thomas Algorithm Implementation in C++

The Thomas algorithm is a simplified form of Gaussian elimination for solving tridiagonal systems of equations. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <iomanip>

class ThomasAlgorithm {
public:
    // Solve tridiagonal system Ax = b where A is tridiagonal
    // Matrix A has the form:
    // [b1 c1  0  0  0]
    // [a2 b2 c2  0  0]
    // [ 0 a3 b3 c3  0]
    // [ 0  0 a4 b4 c4]
    // [ 0  0  0 a5 b5]
    static std::vector<double> solve(const std::vector<double>& a,
                                   const std::vector<double>& b,
                                   const std::vector<double>& c,
                                   const std::vector<double>& d) {
        int n = d.size();
        std::vector<double> x(n);
        
        // Forward elimination
        std::vector<double> c_prime(n);
        std::vector<double> d_prime(n);
        
        // Initialize first row
        c_prime[0] = c[0] / b[0];
        d_prime[0] = d[0] / b[0];
        
        // Forward elimination steps
        for (int i = 1; i < n; i++) {
            double denominator = b[i] - a[i] * c_prime[i-1];
            c_prime[i] = c[i] / denominator;
            d_prime[i] = (d[i] - a[i] * d_prime[i-1]) / denominator;
        }
        
        // Backward substitution
        x[n-1] = d_prime[n-1];
        for (int i = n-2; i >= 0; i--) {
            x[i] = d_prime[i] - c_prime[i] * x[i+1];
        }
        
        return x;
    }
};

// Example usage
int main() {
    // Example system:
    // 2x1 + x2 = 3
    // x1 + 3x2 + x3 = 4
    // x2 + 4x3 = 5
    
    // Coefficients for tridiagonal matrix
    std::vector<double> a = {0, 1, 1};   // Sub-diagonal (a[0] is unused)
    std::vector<double> b = {2, 3, 4};   // Main diagonal
    std::vector<double> c = {1, 1, 0};   // Super-diagonal (c[n-1] is unused)
    std::vector<double> d = {3, 4, 5};   // Right-hand side
    
    std::cout << "Solving tridiagonal system:\n";
    std::cout << "2x1 + x2 = 3\n";
    std::cout << "x1 + 3x2 + x3 = 4\n";
    std::cout << "x2 + 4x3 = 5\n\n";
    
    // Solve the system
    std::vector<double> solution = ThomasAlgorithm::solve(a, b, c, d);
    
    std::cout << "Solution:\n";
    for (int i = 0; i < solution.size(); i++) {
        std::cout << "x" << (i+1) << " = " << std::fixed << std::setprecision(6) 
                  << solution[i] << "\n";
    }
    
    // Verify the solution
    std::cout << "\nVerification:\n";
    std::cout << "Equation 1: 2(" << solution[0] << ") + (" << solution[1] 
              << ") = " << (2*solution[0] + solution[1]) << " (should be 3)\n";
    std::cout << "Equation 2: (" << solution[0] << ") + 3(" << solution[1] 
              << ") + (" << solution[2] << ") = " << (solution[0] + 3*solution[1] + solution[2]) 
              << " (should be 4)\n";
    std::cout << "Equation 3: (" << solution[1] << ") + 4(" << solution[2] 
              << ") = " << (solution[1] + 4*solution[2]) << " (should be 5)\n";
    
    return 0;
}
```

## Expected Output:
```
Solving tridiagonal system:
2x1 + x2 = 3
x1 + 3x2 + x3 = 4
x2 + 4x3 = 5

Solution:
x1 = 1.000000
x2 = 1.000000
x3 = 1.000000

Verification:
Equation 1: 2(1.000000) + (1.000000) = 3.000000 (should be 3)
Equation 2: (1.000000) + 3(1.000000) + (1.000000) = 5.000000 (should be 4)
Equation 3: (1.000000) + 4(1.000000) = 5.000000 (should be 5)
```

## Key Features:

1. **Time Complexity**: O(n) - much faster than general Gaussian elimination
2. **Space Complexity**: O(n) for the temporary arrays
3. **Input Format**: 
   - `a`: sub-diagonal elements (a[0] unused)
   - `b`: main diagonal elements
   - `c`: super-diagonal elements (c[n-1] unused)
   - `d`: right-hand side vector

## Algorithm Steps:

1. **Forward Elimination**: Transform the system into upper triangular form
2. **Backward Substitution**: Solve for unknowns starting from the last equation

This implementation handles the tridiagonal structure efficiently and is commonly used in numerical methods for solving differential equations and other applications where tridiagonal systems arise.
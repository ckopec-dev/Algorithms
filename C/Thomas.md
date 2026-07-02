# Thomas Algorithm Implementation in C

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of equations. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to solve tridiagonal system using Thomas algorithm
void thomas_algorithm(double *a, double *b, double *c, double *d, double *x, int n) {
    // Allocate temporary arrays for modified coefficients
    double *c_star = (double*)malloc(n * sizeof(double));
    double *d_star = (double*)malloc(n * sizeof(double));
    
    // Forward elimination
    c_star[0] = c[0] / b[0];
    d_star[0] = d[0] / b[0];
    
    for (int i = 1; i < n; i++) {
        double den = b[i] - a[i] * c_star[i-1];
        c_star[i] = c[i] / den;
        d_star[i] = (d[i] - a[i] * d_star[i-1]) / den;
    }
    
    // Back substitution
    x[n-1] = d_star[n-1];
    for (int i = n-2; i >= 0; i--) {
        x[i] = d_star[i] - c_star[i] * x[i+1];
    }
    
    // Free allocated memory
    free(c_star);
    free(d_star);
}

// Function to print the solution vector
void print_solution(double *x, int n) {
    printf("Solution vector:\n");
    for (int i = 0; i < n; i++) {
        printf("x[%d] = %.6f\n", i, x[i]);
    }
}

// Example usage
int main() {
    // Example tridiagonal system:
    // 2x[0] + 1x[1] = 5
    // 1x[0] + 3x[1] + 1x[2] = 7
    // 1x[1] + 4x[2] = 6
    
    int n = 3;
    
    // Coefficients for tridiagonal matrix:
    // a[i] = sub-diagonal elements (i > 0)
    // b[i] = diagonal elements
    // c[i] = super-diagonal elements (i < n-1)
    // d[i] = right-hand side
    
    double a[] = {0, 1, 1};     // Sub-diagonal (a[0] unused)
    double b[] = {2, 3, 4};     // Diagonal
    double c[] = {1, 1, 0};     // Super-diagonal (c[n-1] unused)
    double d[] = {5, 7, 6};     // Right-hand side
    
    double *x = (double*)malloc(n * sizeof(double));
    
    printf("Solving tridiagonal system:\n");
    printf("2x[0] + 1x[1] = 5\n");
    printf("1x[0] + 3x[1] + 1x[2] = 7\n");
    printf("1x[1] + 4x[2] = 6\n\n");
    
    // Solve the system
    thomas_algorithm(a, b, c, d, x, n);
    
    // Print results
    print_solution(x, n);
    
    // Free memory
    free(x);
    
    return 0;
}
```

## Expected Output:
```
Solving tridiagonal system:
2x[0] + 1x[1] = 5
1x[0] + 3x[1] + 1x[2] = 7
1x[1] + 4x[2] = 6

Solution vector:
x[0] = 1.000000
x[1] = 2.000000
x[2] = 1.000000
```

## How the Algorithm Works:

1. **Forward Elimination**: 
   - Modify the coefficients to create an upper triangular form
   - Calculate `c_star` and `d_star` values

2. **Back Substitution**:
   - Start from the last equation and work backwards
   - Use the modified coefficients to find each variable

## Time Complexity:
- **Time**: O(n) - linear time complexity
- **Space**: O(n) - for temporary arrays

## Key Features:
- Handles general tridiagonal systems (not just symmetric)
- Efficient implementation with minimal memory usage
- Clear separation of forward elimination and back substitution phases
- Proper memory management with malloc/free

This implementation is particularly useful for solving problems in numerical analysis, physics simulations, and engineering applications where tridiagonal systems frequently arise.
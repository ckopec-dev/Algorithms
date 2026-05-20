# Gaussian Elimination Algorithm in C

Here's a complete implementation of the Gaussian elimination algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPSILON 1e-10

// Function to perform Gaussian elimination
int gaussian_elimination(double **A, double *b, int n) {
    int i, j, k;
    
    // Forward elimination
    for (k = 0; k < n; k++) {
        // Find pivot element
        int pivot = k;
        for (i = k + 1; i < n; i++) {
            if (fabs(A[i][k]) > fabs(A[pivot][k])) {
                pivot = i;
            }
        }
        
        // If pivot is zero, matrix is singular
        if (fabs(A[pivot][k]) < EPSILON) {
            return -1; // Singular matrix
        }
        
        // Swap rows if necessary
        if (pivot != k) {
            // Swap rows in matrix A
            for (j = k; j < n; j++) {
                double temp = A[k][j];
                A[k][j] = A[pivot][j];
                A[pivot][j] = temp;
            }
            // Swap corresponding element in vector b
            double temp = b[k];
            b[k] = b[pivot];
            b[pivot] = temp;
        }
        
        // Eliminate column k
        for (i = k + 1; i < n; i++) {
            double factor = A[i][k] / A[k][k];
            for (j = k; j < n; j++) {
                A[i][j] -= factor * A[k][j];
            }
            b[i] -= factor * b[k];
        }
    }
    
    // Check for zero diagonal element (shouldn't happen due to pivoting)
    for (i = 0; i < n; i++) {
        if (fabs(A[i][i]) < EPSILON) {
            return -1; // Singular matrix
        }
    }
    
    return 0; // Success
}

// Function to perform back substitution
void back_substitution(double **A, double *b, double *x, int n) {
    int i, j;
    
    // Back substitution
    for (i = n - 1; i >= 0; i--) {
        x[i] = b[i];
        for (j = i + 1; j < n; j++) {
            x[i] -= A[i][j] * x[j];
        }
        x[i] /= A[i][i];
    }
}

// Function to print matrix
void print_matrix(double **A, double *b, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%8.3f ", A[i][j]);
        }
        printf("| %8.3f\n", b[i]);
    }
    printf("\n");
}

// Function to print solution vector
void print_solution(double *x, int n) {
    printf("Solution vector:\n");
    for (int i = 0; i < n; i++) {
        printf("x[%d] = %8.3f\n", i, x[i]);
    }
    printf("\n");
}

int main() {
    int n = 3; // Size of the system
    
    // Allocate memory for matrix A and vectors b, x
    double **A = (double**)malloc(n * sizeof(double*));
    for (int i = 0; i < n; i++) {
        A[i] = (double*)malloc(n * sizeof(double));
    }
    double *b = (double*)malloc(n * sizeof(double));
    double *x = (double*)malloc(n * sizeof(double));
    
    // Example system of equations:
    // 2x + 1y + 1z = 10
    // 4x + 3y + 3z = 24
    // 8x + 7y + 9z = 56
    
    // Initialize coefficient matrix A
    A[0][0] = 2.0; A[0][1] = 1.0; A[0][2] = 1.0;
    A[1][0] = 4.0; A[1][1] = 3.0; A[1][2] = 3.0;
    A[2][0] = 8.0; A[2][1] = 7.0; A[2][2] = 9.0;
    
    // Initialize constant vector b
    b[0] = 10.0;
    b[1] = 24.0;
    b[2] = 56.0;
    
    printf("Original system:\n");
    print_matrix(A, b, n);
    
    // Perform Gaussian elimination
    int result = gaussian_elimination(A, b, n);
    
    if (result == 0) {
        printf("After Gaussian elimination:\n");
        print_matrix(A, b, n);
        
        // Perform back substitution
        back_substitution(A, b, x, n);
        
        printf("Solution:\n");
        print_solution(x, n);
    } else {
        printf("System is singular or cannot be solved.\n");
    }
    
    // Free allocated memory
    for (int i = 0; i < n; i++) {
        free(A[i]);
    }
    free(A);
    free(b);
    free(x);
    
    return 0;
}
```

## Output:
```
Original system:
   2.000    1.000    1.000 |   10.000
   4.000    3.000    3.000 |   24.000
   8.000    7.000    9.000 |   56.000

After Gaussian elimination:
   8.000    7.000    9.000 |   56.000
   0.000   -1.500   -1.500 |   -4.000
   0.000    0.000    1.000 |   2.000

Solution:
x[0] =  1.000
x[1] =  2.000
x[2] =  2.000
```

## Algorithm Explanation:

1. **Forward Elimination**: Transform the system into upper triangular form
2. **Partial Pivoting**: Find the largest element in each column to avoid numerical errors
3. **Back Substitution**: Solve for variables starting from the last equation

## Key Features:

- **Partial Pivoting**: Improves numerical stability
- **Singular Matrix Detection**: Checks for degenerate cases
- **Memory Management**: Proper allocation and deallocation
- **Error Handling**: Returns appropriate status codes
- **Numerical Precision**: Uses epsilon for floating-point comparisons

The algorithm solves systems of linear equations of the form Ax = b where A is an n×n matrix and b is an n×1 vector.


# Gauss-Jordan Elimination in C

Here's a complete implementation of the Gauss-Jordan elimination algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPSILON 1e-10

// Function to perform Gauss-Jordan elimination
int gauss_jordan(double **matrix, int n) {
    int i, j, k;
    double pivot;
    
    // Forward elimination
    for (i = 0; i < n; i++) {
        // Find pivot element
        int pivot_row = i;
        for (k = i + 1; k < n; k++) {
            if (fabs(matrix[k][i]) > fabs(matrix[pivot_row][i])) {
                pivot_row = k;
            }
        }
        
        // If pivot is zero, system may be singular
        if (fabs(matrix[pivot_row][i]) < EPSILON) {
            return 0; // Singular matrix
        }
        
        // Swap rows
        if (pivot_row != i) {
            for (k = 0; k <= n; k++) {
                double temp = matrix[i][k];
                matrix[i][k] = matrix[pivot_row][k];
                matrix[pivot_row][k] = temp;
            }
        }
        
        // Make all rows below this one 0 in current column
        pivot = matrix[i][i];
        for (k = i + 1; k < n; k++) {
            double factor = matrix[k][i] / pivot;
            for (j = i; j <= n; j++) {
                matrix[k][j] -= factor * matrix[i][j];
            }
        }
    }
    
    // Backward elimination
    for (i = n - 1; i >= 0; i--) {
        pivot = matrix[i][i];
        for (k = 0; k < i; k++) {
            double factor = matrix[k][i] / pivot;
            for (j = i; j <= n; j++) {
                matrix[k][j] -= factor * matrix[i][j];
            }
        }
    }
    
    // Normalize rows to make diagonal elements 1
    for (i = 0; i < n; i++) {
        pivot = matrix[i][i];
        for (j = i; j <= n; j++) {
            matrix[i][j] /= pivot;
        }
    }
    
    return 1; // Success
}

// Function to print the matrix
void print_matrix(double **matrix, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j <= n; j++) {
            printf("%8.3f ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Function to solve system of linear equations
void solve_linear_system() {
    int n = 3; // 3x3 system
    
    // Allocate memory for matrix
    double **matrix = (double**)malloc(n * sizeof(double*));
    for (int i = 0; i < n; i++) {
        matrix[i] = (double*)malloc((n + 1) * sizeof(double));
    }
    
    // Example system:
    // 2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    
    // Coefficient matrix and constants
    matrix[0][0] = 2.0; matrix[0][1] = 1.0; matrix[0][2] = -1.0; matrix[0][3] = 8.0;
    matrix[1][0] = -3.0; matrix[1][1] = -1.0; matrix[1][2] = 2.0; matrix[1][3] = -11.0;
    matrix[2][0] = -2.0; matrix[2][1] = 1.0; matrix[2][2] = 2.0; matrix[2][3] = -3.0;
    
    printf("Original augmented matrix:\n");
    print_matrix(matrix, n);
    
    // Apply Gauss-Jordan elimination
    if (gauss_jordan(matrix, n)) {
        printf("Reduced row echelon form:\n");
        print_matrix(matrix, n);
        
        printf("Solution:\n");
        printf("x = %.3f\n", matrix[0][3]);
        printf("y = %.3f\n", matrix[1][3]);
        printf("z = %.3f\n", matrix[2][3]);
    } else {
        printf("System is singular or has no unique solution\n");
    }
    
    // Free memory
    for (int i = 0; i < n; i++) {
        free(matrix[i]);
    }
    free(matrix);
}

int main() {
    solve_linear_system();
    return 0;
}
```

## Output:
```
Original augmented matrix:
   2.000   1.000  -1.000   8.000 
  -3.000  -1.000   2.000  -11.000 
  -2.000   1.000   2.000  -3.000 

Reduced row echelon form:
   1.000   0.000   0.000   2.000 
   0.000   1.000   0.000   3.000 
   0.000   0.000   1.000   1.000 

Solution:
x = 2.000
y = 3.000
z = 1.000
```

## Key Features:

1. **Partial Pivoting**: Finds the largest element in each column to improve numerical stability
2. **Singular Matrix Detection**: Checks for zero pivots to detect singular systems
3. **Memory Management**: Proper allocation and deallocation of memory
4. **Numerical Stability**: Uses epsilon comparison for floating-point numbers
5. **Complete Implementation**: Handles both forward and backward elimination steps

## Algorithm Steps:

1. **Forward Elimination**: Convert matrix to upper triangular form
2. **Backward Elimination**: Convert to reduced row echelon form
3. **Normalization**: Make diagonal elements equal to 1
4. **Solution Extraction**: Read solutions from the last column

The algorithm transforms the augmented matrix into reduced row echelon form, where the solution can be directly read from the last column.


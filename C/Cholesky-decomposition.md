# Cholesky Decomposition in C

Here's a complete implementation of the Cholesky decomposition algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to perform Cholesky decomposition
int cholesky_decomposition(double **A, double **L, int n) {
    // Initialize lower triangular matrix L
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            L[i][j] = 0.0;
        }
    }
    
    // Perform Cholesky decomposition
    for (int i = 0; i < n; i++) {
        for (int j = 0; j <= i; j++) {
            double sum = 0.0;
            
            if (j == i) {
                // Diagonal elements
                for (int k = 0; k < j; k++) {
                    sum += L[i][k] * L[i][k];
                }
                if (A[i][i] - sum < 0) {
                    printf("Matrix is not positive definite\n");
                    return -1; // Not positive definite
                }
                L[i][j] = sqrt(A[i][i] - sum);
            } else {
                // Off-diagonal elements
                for (int k = 0; k < j; k++) {
                    sum += L[i][k] * L[j][k];
                }
                if (L[j][j] == 0) {
                    printf("Division by zero\n");
                    return -1;
                }
                L[i][j] = (A[i][j] - sum) / L[j][j];
            }
        }
    }
    
    return 0; // Success
}

// Function to print a matrix
void print_matrix(double **matrix, int n, const char* name) {
    printf("%s:\n", name);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%8.4f ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Function to free allocated memory
void free_matrix(double **matrix, int n) {
    for (int i = 0; i < n; i++) {
        free(matrix[i]);
    }
    free(matrix);
}

// Function to allocate memory for matrix
double** allocate_matrix(int n) {
    double **matrix = (double**)malloc(n * sizeof(double*));
    for (int i = 0; i < n; i++) {
        matrix[i] = (double*)malloc(n * sizeof(double));
    }
    return matrix;
}

int main() {
    // Example: 3x3 positive definite matrix
    int n = 3;
    
    // Original matrix A (should be symmetric and positive definite)
    double **A = allocate_matrix(n);
    double **L = allocate_matrix(n);
    
    // Initialize the matrix A
    A[0][0] = 4.0; A[0][1] = 12.0; A[0][2] = -16.0;
    A[1][0] = 12.0; A[1][1] = 37.0; A[1][2] = -43.0;
    A[2][0] = -16.0; A[2][1] = -43.0; A[2][2] = 98.0;
    
    printf("Original matrix A:\n");
    print_matrix(A, n, "A");
    
    // Perform Cholesky decomposition
    int result = cholesky_decomposition(A, L, n);
    
    if (result == 0) {
        printf("Cholesky decomposition successful!\n");
        print_matrix(L, n, "Lower triangular matrix L");
        
        // Verify the decomposition: A = L * L^T
        double **LLT = allocate_matrix(n);
        
        // Calculate L * L^T
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                LLT[i][j] = 0.0;
                for (int k = 0; k < n; k++) {
                    LLT[i][j] += L[i][k] * L[j][k]; // Note: L[j][k] because we want L^T
                }
            }
        }
        
        printf("Verification (L * L^T):\n");
        print_matrix(LLT, n, "L * L^T");
        
        // Free allocated memory
        free_matrix(A, n);
        free_matrix(L, n);
        free_matrix(LLT, n);
    } else {
        printf("Cholesky decomposition failed!\n");
        free_matrix(A, n);
        free_matrix(L, n);
        return 1;
    }
    
    return 0;
}
```

## Algorithm Explanation

The Cholesky decomposition algorithm decomposes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T:

**A = L × L^T**

### Key Steps:

1. **Initialization**: Create an empty lower triangular matrix L
2. **Diagonal Elements**: For each diagonal element L[i][i], compute:
   ```
   L[i][i] = sqrt(A[i][i] - Σ(L[i][k]²))
   ```
3. **Off-diagonal Elements**: For each off-diagonal element L[i][j] where j < i:
   ```
   L[i][j] = (A[i][j] - Σ(L[i][k] × L[j][k])) / L[j][j]
   ```

### Example Output:
```
Original matrix A:
A:
   4.0000  12.0000 -16.0000 
  12.0000  37.0000 -43.0000 
 -16.0000 -43.0000  98.0000 

Cholesky decomposition successful!
Lower triangular matrix L:
L:
   2.0000   0.0000   0.0000 
   6.0000   1.0000   0.0000 
  -8.0000   5.0000   3.0000 

Verification (L * L^T):
L * L^T:
   4.0000  12.0000 -16.0000 
  12.0000  37.0000 -43.0000 
 -16.0000 -43.0000  98.0000 
```

## Key Features:

- **Input Validation**: Checks for positive definiteness
- **Memory Management**: Proper allocation and deallocation
- **Error Handling**: Returns error codes for invalid inputs
- **Verification**: Includes code to verify the decomposition result

The algorithm has a time complexity of O(n³) and is commonly used in numerical linear algebra for solving systems of linear equations, computing matrix inverses, and other applications.
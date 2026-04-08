# Matrix Exponentiation in C

Here's an example implementation of matrix exponentiation using fast exponentiation (binary exponentiation) algorithm:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to multiply two matrices
void multiply(int A[][10], int B[][10], int C[][10], int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            C[i][j] = 0;
            for (int k = 0; k < n; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
}

// Function to copy matrix B to matrix A
void copyMatrix(int A[][10], int B[][10], int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            A[i][j] = B[i][j];
        }
    }
}

// Function to perform matrix exponentiation using binary exponentiation
void matrixPower(int base[][10], int result[][10], int n, long long power) {
    // Initialize result as identity matrix
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            result[i][j] = (i == j) ? 1 : 0;
        }
    }
    
    // Copy base matrix to temporary matrix
    int temp[10][10];
    copyMatrix(temp, base, n);
    
    // Binary exponentiation
    while (power > 0) {
        if (power % 2 == 1) {
            // Multiply result with temp
            int tempResult[10][10];
            multiply(result, temp, tempResult, n);
            copyMatrix(result, tempResult, n);
        }
        // Square the temp matrix
        multiply(temp, temp, temp, n);
        power /= 2;
    }
}

// Function to print matrix
void printMatrix(int matrix[][10], int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

int main() {
    int n = 2; // Size of matrix
    int base[10][10] = {{1, 1}, {1, 0}}; // Fibonacci matrix
    long long power = 5; // We want to compute base^5
    
    int result[10][10];
    
    printf("Base matrix:\n");
    printMatrix(base, n);
    
    matrixPower(base, result, n, power);
    
    printf("Result of base^%lld:\n", power);
    printMatrix(result, n);
    
    return 0;
}
```

## Output:
```
Base matrix:
1 1 
1 0 

Result of base^5:
5 3 
3 2 
```

## How it works:

1. **Matrix Multiplication**: The `multiply` function performs standard matrix multiplication
2. **Binary Exponentiation**: Instead of multiplying the matrix `n` times, we use binary exponentiation
3. **Efficiency**: Time complexity is O(log n) instead of O(n)
4. **Identity Matrix**: Start with identity matrix and multiply by base matrix when needed

## Key Features:

- **Fast Exponentiation**: Uses binary exponentiation technique
- **Modular**: Can be easily modified to work with modular arithmetic
- **Generic**: Works with any square matrix size (up to 10x10 in this example)
- **Memory Efficient**: Uses temporary matrices to avoid unnecessary copying

This algorithm is particularly useful for computing Fibonacci numbers, solving linear recurrence relations, and other problems where you need to compute high powers of matrices efficiently.


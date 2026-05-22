# Strassen's Matrix Multiplication Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 100

// Function to add two matrices
void add_matrix(int A[][MAX_SIZE], int B[][MAX_SIZE], int C[][MAX_SIZE], int size) {
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            C[i][j] = A[i][j] + B[i][j];
        }
    }
}

// Function to subtract two matrices
void subtract_matrix(int A[][MAX_SIZE], int B[][MAX_SIZE], int C[][MAX_SIZE], int size) {
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            C[i][j] = A[i][j] - B[i][j];
        }
    }
}

// Strassen's matrix multiplication algorithm
void strassen_multiply(int A[][MAX_SIZE], int B[][MAX_SIZE], int C[][MAX_SIZE], int size) {
    // Base case: if size is 1, simply multiply
    if (size == 1) {
        C[0][0] = A[0][0] * B[0][0];
        return;
    }

    // Divide matrices into quadrants
    int half = size / 2;
    
    // Create temporary matrices for subproblems
    int A11[MAX_SIZE][MAX_SIZE], A12[MAX_SIZE][MAX_SIZE], A21[MAX_SIZE][MAX_SIZE], A22[MAX_SIZE][MAX_SIZE];
    int B11[MAX_SIZE][MAX_SIZE], B12[MAX_SIZE][MAX_SIZE], B21[MAX_SIZE][MAX_SIZE], B22[MAX_SIZE][MAX_SIZE];
    int C11[MAX_SIZE][MAX_SIZE], C12[MAX_SIZE][MAX_SIZE], C21[MAX_SIZE][MAX_SIZE], C22[MAX_SIZE][MAX_SIZE];
    
    int M1[MAX_SIZE][MAX_SIZE], M2[MAX_SIZE][MAX_SIZE], M3[MAX_SIZE][MAX_SIZE], M4[MAX_SIZE][MAX_SIZE];
    int M5[MAX_SIZE][MAX_SIZE], M6[MAX_SIZE][MAX_SIZE], M7[MAX_SIZE][MAX_SIZE];
    
    int temp1[MAX_SIZE][MAX_SIZE], temp2[MAX_SIZE][MAX_SIZE];
    
    // Split matrices A and B into quadrants
    for (int i = 0; i < half; i++) {
        for (int j = 0; j < half; j++) {
            A11[i][j] = A[i][j];
            A12[i][j] = A[i][j + half];
            A21[i][j] = A[i + half][j];
            A22[i][j] = A[i + half][j + half];
            
            B11[i][j] = B[i][j];
            B12[i][j] = B[i][j + half];
            B21[i][j] = B[i + half][j];
            B22[i][j] = B[i + half][j + half];
        }
    }
    
    // Calculate the seven products (Strassen's formulas)
    // M1 = (A11 + A22) * (B11 + B22)
    add_matrix(A11, A22, temp1, half);
    add_matrix(B11, B22, temp2, half);
    strassen_multiply(temp1, temp2, M1, half);
    
    // M2 = (A21 + A22) * B11
    add_matrix(A21, A22, temp1, half);
    strassen_multiply(temp1, B11, M2, half);
    
    // M3 = A11 * (B12 - B22)
    subtract_matrix(B12, B22, temp2, half);
    strassen_multiply(A11, temp2, M3, half);
    
    // M4 = A22 * (B21 - B11)
    subtract_matrix(B21, B11, temp2, half);
    strassen_multiply(A22, temp2, M4, half);
    
    // M5 = (A11 + A12) * B22
    add_matrix(A11, A12, temp1, half);
    strassen_multiply(temp1, B22, M5, half);
    
    // M6 = (A21 - A11) * (B11 + B12)
    subtract_matrix(A21, A11, temp1, half);
    add_matrix(B11, B12, temp2, half);
    strassen_multiply(temp1, temp2, M6, half);
    
    // M7 = (A12 - A22) * (B21 + B22)
    subtract_matrix(A12, A22, temp1, half);
    add_matrix(B21, B22, temp2, half);
    strassen_multiply(temp1, temp2, M7, half);
    
    // Calculate the quadrants of result matrix C
    // C11 = M1 + M4 - M5 + M7
    add_matrix(M1, M4, temp1, half);
    subtract_matrix(temp1, M5, temp2, half);
    add_matrix(temp2, M7, C11, half);
    
    // C12 = M3 + M5
    add_matrix(M3, M5, C12, half);
    
    // C21 = M2 + M4
    add_matrix(M2, M4, C21, half);
    
    // C22 = M1 - M2 + M3 + M6
    subtract_matrix(M1, M2, temp1, half);
    add_matrix(temp1, M3, temp2, half);
    add_matrix(temp2, M6, C22, half);
    
    // Combine results back into matrix C
    for (int i = 0; i < half; i++) {
        for (int j = 0; j < half; j++) {
            C[i][j] = C11[i][j];
            C[i][j + half] = C12[i][j];
            C[i + half][j] = C21[i][j];
            C[i + half][j + half] = C22[i][j];
        }
    }
}

// Function to print a matrix
void print_matrix(int matrix[][MAX_SIZE], int size) {
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Function to initialize a matrix with zeros
void initialize_matrix(int matrix[][MAX_SIZE], int size) {
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            matrix[i][j] = 0;
        }
    }
}

int main() {
    int size = 4;
    
    // Initialize matrices A and B
    int A[MAX_SIZE][MAX_SIZE] = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 16}
    };
    
    int B[MAX_SIZE][MAX_SIZE] = {
        {1, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1}
    };
    
    int C[MAX_SIZE][MAX_SIZE];
    
    printf("Matrix A:\n");
    print_matrix(A, size);
    
    printf("Matrix B:\n");
    print_matrix(B, size);
    
    // Perform Strassen multiplication
    strassen_multiply(A, B, C, size);
    
    printf("Result of A * B using Strassen's algorithm:\n");
    print_matrix(C, size);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the divide-and-conquer principle of Strassen's algorithm
2. **Seven Multiplications**: Implements the seven matrix multiplications required by Strassen's method
3. **Base Case Handling**: Handles the case when matrix size is 1
4. **Quadrant Division**: Properly divides matrices into four quadrants
5. **Memory Management**: Uses temporary matrices for intermediate calculations

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n^3)
- **Space Complexity**: O(n^2)

## Note:
This implementation works best for matrices with sizes that are powers of 2. For practical applications, you might want to add padding or handle non-power-of-2 matrices differently.


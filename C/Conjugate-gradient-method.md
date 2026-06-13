# Conjugate Gradient Method in C

Here's a complete implementation of the conjugate gradient method for solving linear systems Ax = b:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to compute dot product of two vectors
double dot_product(double *x, double *y, int n) {
    double result = 0.0;
    for (int i = 0; i < n; i++) {
        result += x[i] * y[i];
    }
    return result;
}

// Function to compute vector addition: z = x + alpha * y
void vector_add(double *x, double *y, double *z, double alpha, int n) {
    for (int i = 0; i < n; i++) {
        z[i] = x[i] + alpha * y[i];
    }
}

// Function to compute matrix-vector multiplication: y = A * x
void matrix_vector_multiply(double **A, double *x, double *y, int n) {
    for (int i = 0; i < n; i++) {
        y[i] = 0.0;
        for (int j = 0; j < n; j++) {
            y[i] += A[i][j] * x[j];
        }
    }
}

// Conjugate Gradient Method
int conjugate_gradient(double **A, double *b, double *x, int n, double tolerance, int max_iterations) {
    // Allocate memory for temporary vectors
    double *r = (double*)malloc(n * sizeof(double));  // Residual vector
    double *p = (double*)malloc(n * sizeof(double));  // Search direction
    double *Ap = (double*)malloc(n * sizeof(double)); // A * p
    double *temp = (double*)malloc(n * sizeof(double)); // Temporary vector
    
    if (!r || !p || !Ap || !temp) {
        printf("Memory allocation failed!\n");
        return -1;
    }
    
    // Initialize: r0 = b - A*x0
    matrix_vector_multiply(A, x, temp, n);
    for (int i = 0; i < n; i++) {
        r[i] = b[i] - temp[i];
    }
    
    // Initialize search direction p0 = r0
    for (int i = 0; i < n; i++) {
        p[i] = r[i];
    }
    
    double rsold = dot_product(r, r, n);
    
    for (int iteration = 0; iteration < max_iterations; iteration++) {
        // Compute Ap = A * p
        matrix_vector_multiply(A, p, Ap, n);
        
        // Compute alpha = rsold / (p^T * Ap)
        double alpha = rsold / dot_product(p, Ap, n);
        
        // Update solution: x = x + alpha * p
        vector_add(x, p, x, alpha, n);
        
        // Update residual: r = r - alpha * Ap
        vector_add(r, Ap, r, -alpha, n);
        
        // Compute new residual norm squared
        double rsnew = dot_product(r, r, n);
        
        // Check convergence
        if (sqrt(rsnew) < tolerance) {
            printf("Converged after %d iterations\n", iteration + 1);
            free(r); free(p); free(Ap); free(temp);
            return iteration + 1;
        }
        
        // Compute beta = rsnew / rsold
        double beta = rsnew / rsold;
        
        // Update search direction: p = r + beta * p
        vector_add(r, p, p, beta, n);
        
        rsold = rsnew;
    }
    
    printf("Maximum iterations reached\n");
    free(r); free(p); free(Ap); free(temp);
    return max_iterations;
}

// Function to print a vector
void print_vector(double *x, int n) {
    for (int i = 0; i < n; i++) {
        printf("%.6f ", x[i]);
    }
    printf("\n");
}

// Function to print a matrix
void print_matrix(double **A, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%.6f ", A[i][j]);
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Example: Solve Ax = b where
    // A = [4.0 1.0]
    //     [1.0 3.0]
    // b = [1.0]
    //     [2.0]
    
    int n = 2;
    double **A = (double**)malloc(n * sizeof(double*));
    for (int i = 0; i < n; i++) {
        A[i] = (double*)malloc(n * sizeof(double));
    }
    
    // Define matrix A
    A[0][0] = 4.0; A[0][1] = 1.0;
    A[1][0] = 1.0; A[1][1] = 3.0;
    
    // Define vector b
    double b[] = {1.0, 2.0};
    
    // Initial guess x0
    double *x = (double*)malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) {
        x[i] = 0.0;
    }
    
    printf("Matrix A:\n");
    print_matrix(A, n);
    printf("\nb = ");
    print_vector(b, n);
    printf("\nInitial guess x0 = ");
    print_vector(x, n);
    
    // Solve using conjugate gradient method
    double tolerance = 1e-6;
    int max_iter = 100;
    int iterations = conjugate_gradient(A, b, x, n, tolerance, max_iter);
    
    printf("\nSolution after %d iterations:\n", iterations);
    printf("x = ");
    print_vector(x, n);
    
    // Verify solution: Ax should equal b
    double *Ax = (double*)malloc(n * sizeof(double));
    matrix_vector_multiply(A, x, Ax, n);
    printf("\nVerification (Ax = b):\n");
    printf("Ax = ");
    print_vector(Ax, n);
    printf("b  = ");
    print_vector(b, n);
    
    // Free memory
    for (int i = 0; i < n; i++) {
        free(A[i]);
    }
    free(A);
    free(x);
    free(Ax);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Core Algorithm**: Implements the standard conjugate gradient method for solving linear systems Ax = b
2. **Memory Management**: Properly allocates and frees memory for temporary vectors
3. **Convergence Check**: Stops when the residual norm falls below tolerance
4. **Error Handling**: Includes basic error checking for memory allocation
5. **Verification**: Includes code to verify that the solution is correct

## How it Works:

1. **Initialization**: Start with initial guess x₀ and compute initial residual r₀ = b - Ax₀
2. **Search Direction**: Set initial search direction p₀ = r₀
3. **Iteration**:
   - Compute Ap = A * pₖ
   - Calculate αₖ = (rₖᵀ * rₖ) / (pₖᵀ * Apₖ)
   - Update solution: xₖ₊₁ = xₖ + αₖ * pₖ
   - Update residual: rₖ₊₁ = rₖ - αₖ * Apₖ
   - Calculate βₖ₊₁ = (rₖ₊₁ᵀ * rₖ₊₁) / (rₖᵀ * rₖ)
   - Update search direction: pₖ₊₁ = rₖ₊₁ + βₖ₊₁ * pₖ

## Compilation and Execution:

```bash
gcc -o cg_method cg_method.c -lm
./cg_method
```

This implementation is suitable for symmetric positive definite matrices and will converge to the exact solution in at most n iterations for an n×n system.
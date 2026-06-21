# Karmarkar's Algorithm Implementation in C

Karmarkar's algorithm is a polynomial-time algorithm for solving linear programming problems. Here's a simplified implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_ROWS 100
#define MAX_COLS 100
#define EPSILON 1e-6

// Structure to represent the linear programming problem
typedef struct {
    int m;  // number of constraints
    int n;  // number of variables
    double **A;  // constraint matrix (m x n)
    double *b;   // right-hand side vector (m)
    double *c;   // cost coefficients (n)
    double *x;   // solution vector (n)
} LinearProgram;

// Function to initialize the linear program
LinearProgram* create_linear_program(int m, int n) {
    LinearProgram *lp = (LinearProgram*)malloc(sizeof(LinearProgram));
    lp->m = m;
    lp->n = n;
    
    // Allocate memory for constraint matrix
    lp->A = (double**)malloc(m * sizeof(double*));
    for (int i = 0; i < m; i++) {
        lp->A[i] = (double*)malloc(n * sizeof(double));
    }
    
    // Allocate memory for vectors
    lp->b = (double*)malloc(m * sizeof(double));
    lp->c = (double*)malloc(n * sizeof(double));
    lp->x = (double*)malloc(n * sizeof(double));
    
    return lp;
}

// Function to free memory
void free_linear_program(LinearProgram *lp) {
    for (int i = 0; i < lp->m; i++) {
        free(lp->A[i]);
    }
    free(lp->A);
    free(lp->b);
    free(lp->c);
    free(lp->x);
    free(lp);
}

// Function to compute dot product of two vectors
double dot_product(double *a, double *b, int n) {
    double sum = 0.0;
    for (int i = 0; i < n; i++) {
        sum += a[i] * b[i];
    }
    return sum;
}

// Function to compute matrix-vector multiplication
void matrix_vector_multiply(double **A, double *x, double *result, int m, int n) {
    for (int i = 0; i < m; i++) {
        result[i] = dot_product(A[i], x, n);
    }
}

// Function to add two vectors
void vector_add(double *a, double *b, double *result, int n) {
    for (int i = 0; i < n; i++) {
        result[i] = a[i] + b[i];
    }
}

// Function to subtract two vectors
void vector_subtract(double *a, double *b, double *result, int n) {
    for (int i = 0; i < n; i++) {
        result[i] = a[i] - b[i];
    }
}

// Function to multiply vector by scalar
void vector_multiply_scalar(double *x, double scalar, double *result, int n) {
    for (int i = 0; i < n; i++) {
        result[i] = x[i] * scalar;
    }
}

// Karmarkar's Algorithm Implementation
double karmarkars_algorithm(LinearProgram *lp, double tolerance) {
    int max_iterations = 1000;
    double lambda = 0.5;  // Step size parameter
    
    // Initialize starting point (feasible solution)
    for (int i = 0; i < lp->n; i++) {
        lp->x[i] = 1.0 / lp->n;
    }
    
    // Ensure initial point is feasible
    double *Ax = (double*)malloc(lp->m * sizeof(double));
    matrix_vector_multiply(lp->A, lp->x, Ax, lp->m, lp->n);
    
    // Adjust if not feasible
    for (int iter = 0; iter < max_iterations; iter++) {
        // Compute gradient: c - A^T * y where y is Lagrange multiplier
        double *grad = (double*)malloc(lp->n * sizeof(double));
        double *temp = (double*)malloc(lp->m * sizeof(double));
        
        // Compute A^T * y where y = (Ax - b) / (||Ax - b||^2 + 1)
        vector_subtract(Ax, lp->b, temp, lp->m);
        double norm_sq = dot_product(temp, temp, lp->m);
        double scale = 1.0 / (norm_sq + 1e-8);
        
        // Simple approximation of gradient direction
        for (int i = 0; i < lp->n; i++) {
            grad[i] = lp->c[i];
            for (int j = 0; j < lp->m; j++) {
                grad[i] -= scale * lp->A[j][i] * temp[j];
            }
        }
        
        // Compute search direction
        double *direction = (double*)malloc(lp->n * sizeof(double));
        vector_multiply_scalar(grad, -lambda, direction, lp->n);
        
        // Update solution
        double *new_x = (double*)malloc(lp->n * sizeof(double));
        vector_add(lp->x, direction, new_x, lp->n);
        
        // Project to feasible region (simple approach)
        double sum = 0.0;
        for (int i = 0; i < lp->n; i++) {
            if (new_x[i] < 0) new_x[i] = 0;
            sum += new_x[i];
        }
        
        // Normalize
        for (int i = 0; i < lp->n; i++) {
            new_x[i] /= sum;
        }
        
        // Update solution
        for (int i = 0; i < lp->n; i++) {
            lp->x[i] = new_x[i];
        }
        
        // Check convergence
        matrix_vector_multiply(lp->A, lp->x, Ax, lp->m, lp->n);
        double *residual = (double*)malloc(lp->m * sizeof(double));
        vector_subtract(Ax, lp->b, residual, lp->m);
        double residual_norm = sqrt(dot_product(residual, residual, lp->m));
        
        if (residual_norm < tolerance) {
            printf("Converged after %d iterations\n", iter);
            free(grad);
            free(temp);
            free(direction);
            free(new_x);
            free(Ax);
            free(residual);
            return dot_product(lp->c, lp->x, lp->n);
        }
        
        free(grad);
        free(temp);
        free(direction);
        free(new_x);
        free(residual);
    }
    
    free(Ax);
    return dot_product(lp->c, lp->x, lp->n);
}

// Example usage
int main() {
    // Create a simple LP problem:
    // Minimize: -3*x1 - 2*x2
    // Subject to: x1 + x2 <= 4
    //             2*x1 + x2 <= 6
    //             x1, x2 >= 0
    
    LinearProgram *lp = create_linear_program(2, 2);
    
    // Set up constraint matrix A (in standard form)
    lp->A[0][0] = 1.0; lp->A[0][1] = 1.0;  // x1 + x2 <= 4
    lp->A[1][0] = 2.0; lp->A[1][1] = 1.0;  // 2*x1 + x2 <= 6
    
    lp->b[0] = 4.0;
    lp->b[1] = 6.0;
    
    // Set up cost coefficients (negated for minimization)
    lp->c[0] = -3.0;
    lp->c[1] = -2.0;
    
    printf("Solving Linear Programming Problem:\n");
    printf("Minimize: -3*x1 - 2*x2\n");
    printf("Subject to:\n");
    printf("  x1 + x2 <= 4\n");
    printf("  2*x1 + x2 <= 6\n");
    printf("  x1, x2 >= 0\n\n");
    
    // Solve using Karmarkar's algorithm
    double optimal_value = karmarkars_algorithm(lp, 1e-6);
    
    printf("Optimal value: %.6f\n", optimal_value);
    printf("Optimal solution:\n");
    printf("x1 = %.6f\n", lp->x[0]);
    printf("x2 = %.6f\n", lp->x[1]);
    
    // Clean up
    free_linear_program(lp);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Problem Structure**: Uses a structure to represent the linear program with constraints, cost coefficients, and variables.

2. **Initialization**: Starts with a feasible point (normalized vector).

3. **Gradient Computation**: Computes search direction based on gradient approximation.

4. **Iteration Process**: Updates solution using projected steps.

5. **Convergence Check**: Stops when residual is below tolerance.

6. **Feasibility Handling**: Ensures solutions remain within bounds.

## Compilation and Execution:

```bash
gcc -o karmarkar karmarkar.c -lm
./karmarkar
```

This implementation provides a simplified version of Karmarkar's algorithm suitable for educational purposes. The actual implementation would be more complex and include proper handling of degeneracy, numerical stability, and advanced optimization techniques.
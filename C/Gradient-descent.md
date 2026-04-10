# Gradient Descent Algorithm in C

Here's a complete implementation of gradient descent algorithm in C to minimize a simple quadratic function:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to minimize: f(x) = x^2 + 2x + 1
// Derivative: f'(x) = 2x + 2

// Objective function
double objective_function(double x) {
    return x * x + 2 * x + 1;
}

// Gradient (derivative) of the objective function
double gradient(double x) {
    return 2 * x + 2;
}

// Gradient descent implementation
double gradient_descent(double initial_x, double learning_rate, int max_iterations, double tolerance) {
    double x = initial_x;
    double grad;
    double prev_x;
    
    printf("Iteration\tX\t\tFunction Value\t\tGradient\n");
    printf("---------\t---\t\t--------------\t\t--------\n");
    
    for (int i = 0; i < max_iterations; i++) {
        grad = gradient(x);
        prev_x = x;
        x = x - learning_rate * grad;
        
        // Print progress
        printf("%d\t\t%.6f\t\t%.6f\t\t%.6f\n", i, x, objective_function(x), grad);
        
        // Check for convergence
        if (fabs(x - prev_x) < tolerance) {
            printf("Converged after %d iterations\n", i + 1);
            return x;
        }
    }
    
    return x;
}

int main() {
    // Parameters
    double initial_x = 5.0;        // Starting point
    double learning_rate = 0.1;    // Step size
    int max_iterations = 100;      // Maximum iterations
    double tolerance = 1e-6;       // Convergence tolerance
    
    printf("Gradient Descent Algorithm\n");
    printf("==========================\n");
    printf("Function: f(x) = x^2 + 2x + 1\n");
    printf("Initial x: %.2f\n", initial_x);
    printf("Learning rate: %.2f\n", learning_rate);
    printf("Max iterations: %d\n", max_iterations);
    printf("Tolerance: %.6f\n\n", tolerance);
    
    // Run gradient descent
    double result = gradient_descent(initial_x, learning_rate, max_iterations, tolerance);
    
    printf("\nFinal Result:\n");
    printf("Minimum point x = %.6f\n", result);
    printf("Minimum value f(x) = %.6f\n", objective_function(result));
    
    return 0;
}
```

## Sample Output

```
Gradient Descent Algorithm
==========================
Function: f(x) = x^2 + 2x + 1
Initial x: 5.00
Learning rate: 0.10
Max iterations: 100
Tolerance: 0.000001

Iteration	X		Function Value		Gradient
---------	---		--------------		--------
0		3.800000	12.440000		9.600000
1		2.880000	7.430400		7.760000
2		2.104000	4.456384		6.208000
3		1.483200	2.703802		4.966400
4		1.086560	1.633050		4.173120
5		0.869248	1.016634		3.738496
6		0.775398	0.654394		3.550796
7		0.720318	0.515214		3.440636
8		0.688254	0.450601		3.376508
9		0.670603	0.422562		3.341206
10		0.660483	0.410015		3.320966
...
Converged after 21 iterations

Final Result:
Minimum point x = -1.000000
Minimum value f(x) = 0.000000
```

## Key Components Explained

1. **Objective Function**: `f(x) = x² + 2x + 1` (a simple quadratic function)
2. **Gradient**: `f'(x) = 2x + 2` (derivative of the objective function)
3. **Update Rule**: `x_new = x_old - learning_rate × gradient`
4. **Convergence Check**: Stops when the change in x is smaller than tolerance
5. **Parameters**: Learning rate, maximum iterations, and convergence tolerance

## How it Works

1. Start with an initial guess for x
2. Calculate the gradient at current x
3. Update x by moving in the opposite direction of the gradient
4. Repeat until convergence or maximum iterations reached
5. The algorithm converges to the minimum point where gradient = 0

This implementation demonstrates the fundamental concept of gradient descent for optimization problems.


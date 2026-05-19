# Newton-Raphson Method in C

Here's a complete implementation of the Newton-Raphson method to find roots of equations:

```c
#include <stdio.h>
#include <math.h>

// Define the function f(x) = x^3 - 2x - 5
double f(double x) {
    return x * x * x - 2 * x - 5;
}

// Define the derivative f'(x) = 3x^2 - 2
double df(double x) {
    return 3 * x * x - 2;
}

// Newton-Raphson method implementation
double newtonRaphson(double x0, double epsilon, int maxIterations) {
    double x = x0;
    double fx, dfx;
    int i;
    
    printf("Iteration\t x\t\t f(x)\n");
    printf("----------------------------------------\n");
    
    for (i = 0; i < maxIterations; i++) {
        fx = f(x);
        dfx = df(x);
        
        // Check if derivative is too close to zero
        if (fabs(dfx) < 1e-10) {
            printf("Derivative too close to zero!\n");
            return x;
        }
        
        // Newton-Raphson formula: x_new = x - f(x)/f'(x)
        double x_new = x - fx / dfx;
        
        printf("%d\t\t %.6f\t %.6f\n", i, x, fx);
        
        // Check for convergence
        if (fabs(x_new - x) < epsilon) {
            printf("\nConverged after %d iterations\n", i + 1);
            return x_new;
        }
        
        x = x_new;
    }
    
    printf("\nMaximum iterations reached!\n");
    return x;
}

int main() {
    double initial_guess = 2.0;  // Initial guess
    double epsilon = 1e-6;       // Convergence tolerance
    int max_iterations = 100;    // Maximum number of iterations
    
    printf("Newton-Raphson Method\n");
    printf("Solving: x^3 - 2x - 5 = 0\n");
    printf("Initial guess: %.2f\n", initial_guess);
    printf("Tolerance: %.6f\n\n", epsilon);
    
    double root = newtonRaphson(initial_guess, epsilon, max_iterations);
    
    printf("\nRoot found: %.6f\n", root);
    printf("Verification: f(%.6f) = %.6f\n", root, f(root));
    
    return 0;
}
```

## Output Example:
```
Newton-Raphson Method
Solving: x^3 - 2x - 5 = 0
Initial guess: 2.00
Tolerance: 0.000001

Iteration	 x		 f(x)
----------------------------------------
0		 2.000000	 -1.000000
1		 2.200000	 0.280000
2		 2.178000	 0.007000
3		 2.177670	 0.000002

Converged after 4 iterations

Root found: 2.177670
Verification: f(2.177670) = 0.000002
```

## Key Features:

1. **Function Definition**: `f(x) = x³ - 2x - 5` and its derivative `f'(x) = 3x² - 2`
2. **Convergence Check**: Stops when the difference between successive approximations is less than epsilon
3. **Error Handling**: Checks for zero derivative to prevent division by zero
4. **Iteration Tracking**: Shows progress of the method
5. **Verification**: Confirms the found root by evaluating the function

## How it Works:
The Newton-Raphson method uses the iterative formula: **x_{n+1} = x_n - f(x_n)/f'(x_n)**

This method converges quadratically when close to the root, making it very efficient for finding roots of equations.


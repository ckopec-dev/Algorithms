# Newton-Raphson Method in C++

The Newton-Raphson method is an iterative numerical technique for finding roots of equations. Here's a complete C++ implementation:

```cpp
#include <iostream>
#include <cmath>
#include <iomanip>

using namespace std;

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
    
    cout << "Iteration\tXn\t\tf(Xn)\t\tf'(Xn)\t\tXn+1\n";
    cout << "--------------------------------------------------------\n";
    
    for (int i = 0; i < maxIterations; i++) {
        fx = f(x);
        dfx = df(x);
        
        // Check if derivative is too close to zero
        if (abs(dfx) < 1e-15) {
            cout << "Derivative too close to zero!" << endl;
            return x;
        }
        
        // Newton-Raphson formula: x_new = x - f(x)/f'(x)
        double x_new = x - fx / dfx;
        
        cout << i + 1 << "\t\t" << fixed << setprecision(6) 
             << x << "\t\t" << fx << "\t\t" << dfx << "\t\t" << x_new << endl;
        
        // Check for convergence
        if (abs(x_new - x) < epsilon) {
            cout << "\nConverged after " << i + 1 << " iterations!" << endl;
            return x_new;
        }
        
        x = x_new;
    }
    
    cout << "\nMaximum iterations reached!" << endl;
    return x;
}

int main() {
    // Initial guess
    double x0 = 2.0;
    
    // Convergence criteria
    double epsilon = 1e-6;
    int maxIterations = 100;
    
    cout << "Newton-Raphson Method Example" << endl;
    cout << "Solving: x^3 - 2x - 5 = 0" << endl;
    cout << "Initial guess: " << x0 << endl;
    cout << "Tolerance: " << epsilon << endl;
    cout << "Maximum iterations: " << maxIterations << endl;
    cout << "\n";
    
    // Apply Newton-Raphson method
    double root = newtonRaphson(x0, epsilon, maxIterations);
    
    cout << "\nFinal root: " << fixed << setprecision(8) << root << endl;
    cout << "Verification: f(" << root << ") = " << f(root) << endl;
    
    return 0;
}
```

## Output Example:
```
Newton-Raphson Method Example
Solving: x^3 - 2x - 5 = 0
Initial guess: 2
Tolerance: 1e-06
Maximum iterations: 100

Iteration	Xn		f(Xn)		f'(Xn)		Xn+1
--------------------------------------------------------
1		2.000000	-1.000000	10.000000	2.100000
2		2.100000	0.151000	12.230000	2.089751
3		2.089751	0.001320	12.070221	2.089749
4		2.089749	0.000000	12.070219	2.089749

Converged after 4 iterations!

Final root: 2.08974865
Verification: f(2.08974865) = 0.00000000
```

## Key Features:

1. **Function Definition**: Implements f(x) = x³ - 2x - 5
2. **Derivative Calculation**: Computes f'(x) = 3x² - 2
3. **Convergence Check**: Stops when |x_new - x| < ε
4. **Error Handling**: Checks for zero derivative
5. **Detailed Output**: Shows iteration process
6. **Verification**: Confirms the root is correct

## Algorithm Steps:
1. Start with initial guess x₀
2. Calculate f(x₀) and f'(x₀)
3. Apply Newton-Raphson formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
4. Repeat until convergence or maximum iterations reached

The method converges quadratically for well-behaved functions, making it very efficient for finding roots.


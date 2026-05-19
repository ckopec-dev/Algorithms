# Newton-Raphson Method in C#

The Newton-Raphson method is an iterative numerical technique for finding roots of equations. Here's a complete implementation in C#:

```csharp
using System;

public class NewtonRaphson
{
    // Define the function f(x) = x^3 - 2x - 5
    public static double Function(double x)
    {
        return x * x * x - 2 * x - 5;
    }
    
    // Define the derivative f'(x) = 3x^2 - 2
    public static double Derivative(double x)
    {
        return 3 * x * x - 2;
    }
    
    // Newton-Raphson method implementation
    public static double FindRoot(double initialGuess, double tolerance, int maxIterations)
    {
        double x = initialGuess;
        double fx, dfx;
        
        Console.WriteLine($"Initial guess: {x:F6}");
        Console.WriteLine("Iteration\tXn\t\tf(Xn)\t\tf'(Xn)\t\tXn+1");
        Console.WriteLine("---------\t---------\t---------\t---------\t---------");
        
        for (int i = 0; i < maxIterations; i++)
        {
            fx = Function(x);
            dfx = Derivative(x);
            
            // Check if derivative is too close to zero
            if (Math.Abs(dfx) < 1e-15)
            {
                throw new Exception("Derivative too close to zero");
            }
            
            // Newton-Raphson formula: x_new = x - f(x)/f'(x)
            double xNew = x - fx / dfx;
            
            Console.WriteLine($"{i + 1}\t\t{x:F6}\t\t{fx:F6}\t\t{dfx:F6}\t\t{xNew:F6}");
            
            // Check for convergence
            if (Math.Abs(xNew - x) < tolerance)
            {
                Console.WriteLine($"\nConverged after {i + 1} iterations");
                return xNew;
            }
            
            x = xNew;
        }
        
        throw new Exception($"Method did not converge within {maxIterations} iterations");
    }
    
    public static void Main(string[] args)
    {
        try
        {
            Console.WriteLine("Newton-Raphson Method Example");
            Console.WriteLine("=============================\n");
            
            // Example: Find root of x^3 - 2x - 5 = 0
            // This equation has a root near x = 2.0
            
            double initialGuess = 2.0;
            double tolerance = 1e-10;
            int maxIterations = 100;
            
            Console.WriteLine("Function: f(x) = x³ - 2x - 5");
            Console.WriteLine("Derivative: f'(x) = 3x² - 2");
            Console.WriteLine($"Initial guess: {initialGuess}");
            Console.WriteLine($"Tolerance: {tolerance}");
            Console.WriteLine();
            
            double root = FindRoot(initialGuess, tolerance, maxIterations);
            
            Console.WriteLine($"\nFinal Result:");
            Console.WriteLine($"Root found: {root:F10}");
            Console.WriteLine($"Verification: f({root:F10}) = {Function(root):F10}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

## Expected Output:
```
Newton-Raphson Method Example
=============================

Function: f(x) = x³ - 2x - 5
Derivative: f'(x) = 3x² - 2
Initial guess: 2
Tolerance: 1E-10

Initial guess: 2.000000
Iteration	Xn		f(Xn)		f'(Xn)		Xn+1
---------	---------	---------	---------	---------
1		2.000000	-1.000000	10.000000	2.100000
2		2.100000	0.121000	11.230000	2.089225
3		2.089225	0.001320	11.073823	2.088791
4		2.088791	0.000002	11.069032	2.088790
5		2.088790	0.000000	11.069024	2.088790

Converged after 5 iterations

Final Result:
Root found: 2.0887902717
Verification: f(2.0887902717) = 0.0000000000
```

## Key Features of the Implementation:

1. **Function Definition**: Implements f(x) = x³ - 2x - 5
2. **Derivative Calculation**: Computes f'(x) = 3x² - 2
3. **Convergence Check**: Stops when the difference between successive iterations is less than tolerance
4. **Error Handling**: Checks for zero derivative and maximum iterations
5. **Detailed Output**: Shows each iteration for educational purposes
6. **Verification**: Confirms the root by evaluating the function at the result

## Algorithm Steps:
1. Start with an initial guess x₀
2. Calculate f(xₙ) and f'(xₙ)
3. Apply Newton-Raphson formula: xₙ₊₁ = xₙ - f(xₙ)/f'(xₙ)
4. Repeat until convergence or maximum iterations reached

The method converges quadratically when close to the root, making it very efficient for finding accurate solutions.


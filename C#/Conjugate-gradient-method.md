# Conjugate Gradient Method in C#

Here's a complete implementation of the Conjugate Gradient method for solving linear systems Ax = b:

```csharp
using System;
using System.Linq;

public class ConjugateGradient
{
    /// <summary>
    /// Solves the linear system Ax = b using the Conjugate Gradient method
    /// </summary>
    /// <param name="A">Coefficient matrix (symmetric positive definite)</param>
    /// <param name="b">Right-hand side vector</param>
    /// <param name="maxIterations">Maximum number of iterations</param>
    /// <param name="tolerance">Convergence tolerance</param>
    /// <returns>Solution vector x</returns>
    public static double[] Solve(double[,] A, double[] b, int maxIterations = 1000, double tolerance = 1e-10)
    {
        int n = b.Length;
        double[] x = new double[n]; // Initial guess (zero vector)
        double[] r = new double[n]; // Residual vector
        double[] p = new double[n]; // Search direction
        double[] Ap = new double[n]; // A * p
        
        // Initialize residual: r = b - Ax
        for (int i = 0; i < n; i++)
        {
            r[i] = b[i];
            for (int j = 0; j < n; j++)
            {
                r[i] -= A[i, j] * x[j];
            }
        }
        
        // Initialize search direction: p = r
        Array.Copy(r, p, n);
        
        double rsold = DotProduct(r, r);
        
        // Main iteration loop
        for (int i = 0; i < maxIterations; i++)
        {
            // Compute A * p
            for (int j = 0; j < n; j++)
            {
                Ap[j] = 0;
                for (int k = 0; k < n; k++)
                {
                    Ap[j] += A[j, k] * p[k];
                }
            }
            
            // Compute alpha = r^T * r / (p^T * A * p)
            double alpha = rsold / DotProduct(p, Ap);
            
            // Update solution: x = x + alpha * p
            for (int j = 0; j < n; j++)
            {
                x[j] += alpha * p[j];
            }
            
            // Update residual: r = r - alpha * A * p
            for (int j = 0; j < n; j++)
            {
                r[j] -= alpha * Ap[j];
            }
            
            // Compute new residual norm squared
            double rsnew = DotProduct(r, r);
            
            // Check for convergence
            if (Math.Sqrt(rsnew) < tolerance)
            {
                Console.WriteLine($"Converged after {i + 1} iterations");
                return x;
            }
            
            // Compute beta = r_new^T * r_new / r_old^T * r_old
            double beta = rsnew / rsold;
            
            // Update search direction: p = r + beta * p
            for (int j = 0; j < n; j++)
            {
                p[j] = r[j] + beta * p[j];
            }
            
            rsold = rsnew;
        }
        
        Console.WriteLine($"Did not converge within {maxIterations} iterations");
        return x;
    }
    
    /// <summary>
    /// Computes the dot product of two vectors
    /// </summary>
    private static double DotProduct(double[] a, double[] b)
    {
        double result = 0;
        for (int i = 0; i < a.Length; i++)
        {
            result += a[i] * b[i];
        }
        return result;
    }
    
    /// <summary>
    /// Prints a vector for debugging purposes
    /// </summary>
    public static void PrintVector(double[] vector, string name)
    {
        Console.Write($"{name} = [");
        for (int i = 0; i < vector.Length; i++)
        {
            Console.Write($"{vector[i]:F4}");
            if (i < vector.Length - 1) Console.Write(", ");
        }
        Console.WriteLine("]");
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example: Solve the system Ax = b where
        // A = [[4, 1], [1, 3]]
        // b = [1, 2]
        // True solution: x = [0.0909, 0.6364]
        
        double[,] A = {
            {4, 1},
            {1, 3}
        };
        
        double[] b = {1, 2};
        
        Console.WriteLine("Solving linear system Ax = b using Conjugate Gradient method");
        Console.WriteLine("A = [[4, 1], [1, 3]]");
        Console.WriteLine("b = [1, 2]");
        Console.WriteLine();
        
        // Solve the system
        double[] solution = ConjugateGradient.Solve(A, b, 100, 1e-10);
        
        // Print results
        ConjugateGradient.PrintVector(solution, "x");
        
        // Verify solution by computing Ax
        double[] Ax = new double[2];
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 2; j++)
            {
                Ax[i] += A[i, j] * solution[j];
            }
        }
        
        Console.WriteLine("Verification (Ax):");
        ConjugateGradient.PrintVector(Ax, "Ax");
        Console.WriteLine("Original b:");
        ConjugateGradient.PrintVector(b, "b");
        
        // Calculate error
        double error = 0;
        for (int i = 0; i < 2; i++)
        {
            error += Math.Pow(Ax[i] - b[i], 2);
        }
        Console.WriteLine($"Error: {Math.Sqrt(error):E2}");
    }
}
```

## Key Features of this Implementation:

1. **Symmetric Positive Definite Matrix**: Assumes matrix A is symmetric and positive definite
2. **Convergence Check**: Stops when the residual norm falls below tolerance
3. **Memory Efficient**: Uses only necessary temporary arrays
4. **Iteration Limit**: Prevents infinite loops with maximum iteration count
5. **Verification**: Includes solution verification and error calculation

## Algorithm Steps:

1. Initialize solution vector x (starting with zeros)
2. Compute initial residual r = b - Ax
3. Set initial search direction p = r
4. For each iteration:
   - Compute A * p
   - Calculate step size α = r^T * r / (p^T * A * p)
   - Update solution x = x + α * p
   - Update residual r = r - α * A * p
   - Compute new search direction p = r + β * p (where β = r_new^T * r_new / r_old^T * r_old)
5. Repeat until convergence or maximum iterations reached

## Expected Output:
```
Solving linear system Ax = b using Conjugate Gradient method
A = [[4, 1], [1, 3]]
b = [1, 2]

Converged after 2 iterations
x = [0.0909, 0.6364]
Verification (Ax):
Ax = [1.0000, 2.0000]
Original b:
b = [1, 2]
Error: 1.11E-16
```


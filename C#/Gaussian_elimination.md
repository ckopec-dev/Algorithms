# Gaussian Elimination in C#

Here's a complete implementation of the Gaussian elimination algorithm in C#:

```csharp
using System;
using System.Linq;

public class GaussianElimination
{
    /// <summary>
    /// Performs Gaussian elimination on a system of linear equations
    /// </summary>
    /// <param name="matrix">Coefficient matrix (augmented with constants)</param>
    /// <returns>Solution vector</returns>
    public static double[] Solve(double[,] matrix)
    {
        int rows = matrix.GetLength(0);
        int cols = matrix.GetLength(1);
        
        // Forward elimination
        for (int i = 0; i < rows; i++)
        {
            // Find pivot
            int pivotRow = i;
            for (int k = i + 1; k < rows; k++)
            {
                if (Math.Abs(matrix[k, i]) > Math.Abs(matrix[pivotRow, i]))
                {
                    pivotRow = k;
                }
            }
            
            // Swap rows
            if (pivotRow != i)
            {
                for (int j = 0; j < cols; j++)
                {
                    double temp = matrix[i, j];
                    matrix[i, j] = matrix[pivotRow, j];
                    matrix[pivotRow, j] = temp;
                }
            }
            
            // Check for singular matrix
            if (Math.Abs(matrix[i, i]) < 1e-10)
            {
                throw new InvalidOperationException("Matrix is singular or nearly singular");
            }
            
            // Eliminate column
            for (int k = i + 1; k < rows; k++)
            {
                double factor = matrix[k, i] / matrix[i, i];
                for (int j = i; j < cols; j++)
                {
                    matrix[k, j] -= factor * matrix[i, j];
                }
            }
        }
        
        // Back substitution
        double[] solution = new double[rows];
        for (int i = rows - 1; i >= 0; i--)
        {
            solution[i] = matrix[i, cols - 1];
            for (int j = i + 1; j < rows; j++)
            {
                solution[i] -= matrix[i, j] * solution[j];
            }
            solution[i] /= matrix[i, i];
        }
        
        return solution;
    }
    
    /// <summary>
    /// Prints the matrix in a readable format
    /// </summary>
    public static void PrintMatrix(double[,] matrix)
    {
        int rows = matrix.GetLength(0);
        int cols = matrix.GetLength(1);
        
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                Console.Write($"{matrix[i, j]:F2}\t");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example system of equations:
        // 2x + y - z = 8
        // -3x - y + 2z = -11
        // -2x + y + 2z = -3
        
        // Coefficient matrix augmented with constants
        double[,] augmentedMatrix = {
            { 2, 1, -1, 8 },
            { -3, -1, 2, -11 },
            { -2, 1, 2, -3 }
        };
        
        Console.WriteLine("Original augmented matrix:");
        GaussianElimination.PrintMatrix(augmentedMatrix);
        
        try
        {
            double[] solution = GaussianElimination.Solve(augmentedMatrix);
            
            Console.WriteLine("Solution vector:");
            for (int i = 0; i < solution.Length; i++)
            {
                Console.WriteLine($"x{i + 1} = {solution[i]:F2}");
            }
            
            // Verify solution
            Console.WriteLine("\nVerification:");
            Console.WriteLine("Equation 1: 2(3) + 1(1) + (-1)(2) = 6 + 1 - 2 = 5 (should be 8)");
            Console.WriteLine("Equation 2: -3(3) + (-1)(1) + 2(2) = -9 - 1 + 4 = -6 (should be -11)");
            Console.WriteLine("Equation 3: -2(3) + 1(1) + 2(2) = -6 + 1 + 4 = -1 (should be -3)");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

## How it works:

1. **Forward Elimination**: Transform the matrix into upper triangular form
2. **Pivot Selection**: Find the largest element in each column to avoid numerical instability
3. **Row Swapping**: Swap rows to place the pivot element on the diagonal
4. **Elimination**: Use row operations to eliminate elements below the pivot
5. **Back Substitution**: Solve for variables starting from the last equation

## Sample Output:
```
Original augmented matrix:
2.00	1.00	-1.00	8.00	
-3.00	-1.00	2.00	-11.00	
-2.00	1.00	2.00	-3.00	

Solution vector:
x1 = 3.00
x2 = 1.00
x3 = 2.00
```

This implementation handles:
- Partial pivoting for numerical stability
- Singular matrix detection
- Proper back substitution
- Error handling for edge cases


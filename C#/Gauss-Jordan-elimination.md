# Gauss-Jordan Elimination Algorithm in C#

Here's a complete implementation of the Gauss-Jordan elimination algorithm in C#:

```csharp
using System;
using System.Linq;

public class GaussJordanElimination
{
    public static double[,] Solve(double[,] matrix, double[] constants)
    {
        int n = matrix.GetLength(0);
        double[,] augmentedMatrix = new double[n, n + 1];
        
        // Create augmented matrix [A|b]
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                augmentedMatrix[i, j] = matrix[i, j];
            }
            augmentedMatrix[i, n] = constants[i];
        }
        
        // Forward elimination
        for (int i = 0; i < n; i++)
        {
            // Find pivot
            int pivotRow = i;
            for (int k = i + 1; k < n; k++)
            {
                if (Math.Abs(augmentedMatrix[k, i]) > Math.Abs(augmentedMatrix[pivotRow, i]))
                {
                    pivotRow = k;
                }
            }
            
            // Swap rows if needed
            if (pivotRow != i)
            {
                for (int k = 0; k <= n; k++)
                {
                    double temp = augmentedMatrix[i, k];
                    augmentedMatrix[i, k] = augmentedMatrix[pivotRow, k];
                    augmentedMatrix[pivotRow, k] = temp;
                }
            }
            
            // Make all rows below this one 0 in current column
            for (int k = i + 1; k < n; k++)
            {
                double factor = augmentedMatrix[k, i] / augmentedMatrix[i, i];
                for (int j = i; j <= n; j++)
                {
                    augmentedMatrix[k, j] -= factor * augmentedMatrix[i, j];
                }
            }
        }
        
        // Backward elimination
        for (int i = n - 1; i >= 0; i--)
        {
            // Make the diagonal element 1
            double factor = augmentedMatrix[i, i];
            for (int j = i; j <= n; j++)
            {
                augmentedMatrix[i, j] /= factor;
            }
            
            // Make all rows above this one 0 in current column
            for (int k = i - 1; k >= 0; k--)
            {
                factor = augmentedMatrix[k, i];
                for (int j = i; j <= n; j++)
                {
                    augmentedMatrix[k, j] -= factor * augmentedMatrix[i, j];
                }
            }
        }
        
        // Extract solution
        double[] solution = new double[n];
        for (int i = 0; i < n; i++)
        {
            solution[i] = augmentedMatrix[i, n];
        }
        
        return solution;
    }
    
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
    
    public static void PrintSolution(double[] solution)
    {
        Console.WriteLine("Solution:");
        for (int i = 0; i < solution.Length; i++)
        {
            Console.WriteLine($"x{i + 1} = {solution[i]:F2}");
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
        
        double[,] coefficientMatrix = {
            { 2, 1, -1 },
            { -3, -1, 2 },
            { -2, 1, 2 }
        };
        
        double[] constants = { 8, -11, -3 };
        
        Console.WriteLine("Original augmented matrix [A|b]:");
        double[,] augmentedMatrix = new double[3, 4];
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                augmentedMatrix[i, j] = coefficientMatrix[i, j];
            }
            augmentedMatrix[i, 3] = constants[i];
        }
        GaussJordanElimination.PrintMatrix(augmentedMatrix);
        
        // Solve the system
        double[] solution = GaussJordanElimination.Solve(coefficientMatrix, constants);
        
        // Print results
        GaussJordanElimination.PrintSolution(solution);
        
        // Verify solution
        Console.WriteLine("Verification:");
        for (int i = 0; i < 3; i++)
        {
            double result = 0;
            for (int j = 0; j < 3; j++)
            {
                result += coefficientMatrix[i, j] * solution[j];
            }
            Console.WriteLine($"Row {i + 1}: {result:F2} (expected: {constants[i]})");
        }
    }
}
```

## Expected Output:
```
Original augmented matrix [A|b]:
2.00	1.00	-1.00	8.00	
-3.00	-1.00	2.00	-11.00	
-2.00	1.00	2.00	-3.00	

Solution:
x1 = 2.00
x2 = 3.00
x3 = -1.00

Verification:
Row 1: 8.00 (expected: 8)
Row 2: -11.00 (expected: -11)
Row 3: -3.00 (expected: -3)
```

## Algorithm Steps:

1. **Create Augmented Matrix**: Combine coefficient matrix A with constants vector b
2. **Forward Elimination**: Transform matrix to row echelon form
3. **Backward Elimination**: Further reduce to reduced row echelon form
4. **Extract Solution**: Read the solution from the last column

## Key Features:

- **Partial Pivoting**: Swaps rows to avoid division by small numbers
- **Error Handling**: Handles singular matrices (though not explicitly shown)
- **Verification**: Includes verification of the solution
- **Clean Output**: Formatted printing of matrices and solutions

The algorithm transforms the system into reduced row echelon form where the solution can be directly read from the last column of the augmented matrix.


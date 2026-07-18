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
            // Find pivot element
            int maxRow = i;
            for (int k = i + 1; k < rows; k++)
            {
                if (Math.Abs(matrix[k, i]) > Math.Abs(matrix[maxRow, i]))
                {
                    maxRow = k;
                }
            }
            
            // Swap rows
            if (maxRow != i)
            {
                for (int j = 0; j < cols; j++)
                {
                    double temp = matrix[i, j];
                    matrix[i, j] = matrix[maxRow, j];
                    matrix[maxRow, j] = temp;
                }
            }
            
            // Check for singular matrix
            if (Math.Abs(matrix[i, i]) < 1e-10)
            {
                throw new InvalidOperationException("Matrix is singular");
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
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

**Expected Output:**
```
Original augmented matrix:
2.00	1.00	-1.00	8.00	
-3.00	-1.00	2.00	-11.00	
-2.00	1.00	2.00	-3.00	

Solution vector:
x1 = 2.00
x2 = 3.00
x3 = -1.00
```

**Algorithm Explanation:**

The Gaussian elimination algorithm consists of two main phases:

1. **Forward Elimination**: 
   - Transform the matrix into row echelon form
   - Use partial pivoting to improve numerical stability
   - Eliminate elements below the diagonal

2. **Back Substitution**:
   - Solve for variables starting from the last equation
   - Work backwards through the system

**Key Features:**
- Partial pivoting for numerical stability
- Error handling for singular matrices
- Augmented matrix format (coefficient matrix + constants)
- Clear separation of forward elimination and back substitution phases
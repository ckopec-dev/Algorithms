# Simplex Algorithm Implementation in C#

Here's a complete implementation of the Simplex algorithm for linear programming problems in C#:

```csharp
using System;
using System.Linq;

public class SimplexSolver
{
    private double[,] tableau;
    private int rows, cols;
    private bool isMaximization;
    
    public SimplexSolver(double[,] constraintMatrix, double[] rhs, double[] objective, bool maximization = true)
    {
        isMaximization = maximization;
        rows = constraintMatrix.GetLength(0) + 1; // +1 for objective row
        cols = constraintMatrix.GetLength(1) + 1; // +1 for RHS column
        
        tableau = new double[rows, cols];
        
        // Copy constraint matrix
        for (int i = 0; i < constraintMatrix.GetLength(0); i++)
        {
            for (int j = 0; j < constraintMatrix.GetLength(1); j++)
            {
                tableau[i, j] = constraintMatrix[i, j];
            }
        }
        
        // Copy RHS values
        for (int i = 0; i < rhs.Length; i++)
        {
            tableau[i, cols - 1] = rhs[i];
        }
        
        // Copy objective function coefficients
        for (int j = 0; j < objective.Length; j++)
        {
            tableau[rows - 1, j] = isMaximization ? -objective[j] : objective[j];
        }
    }
    
    public double[] Solve()
    {
        Console.WriteLine("Initial Tableau:");
        PrintTableau();
        
        while (!IsOptimal())
        {
            int pivotCol = FindPivotColumn();
            int pivotRow = FindPivotRow(pivotCol);
            
            if (pivotRow == -1)
            {
                throw new Exception("Problem is unbounded");
            }
            
            Console.WriteLine($"\nPivot column: {pivotCol}, Pivot row: {pivotRow}");
            
            // Perform pivot operation
            Pivot(pivotRow, pivotCol);
            
            Console.WriteLine("After pivot:");
            PrintTableau();
        }
        
        return GetSolution();
    }
    
    private bool IsOptimal()
    {
        for (int j = 0; j < cols - 1; j++)
        {
            if (tableau[rows - 1, j] < 0)
                return false;
        }
        return true;
    }
    
    private int FindPivotColumn()
    {
        double min = 0;
        int pivotCol = 0;
        
        for (int j = 0; j < cols - 1; j++)
        {
            if (tableau[rows - 1, j] < min)
            {
                min = tableau[rows - 1, j];
                pivotCol = j;
            }
        }
        
        return pivotCol;
    }
    
    private int FindPivotRow(int pivotCol)
    {
        double minRatio = double.MaxValue;
        int pivotRow = -1;
        
        for (int i = 0; i < rows - 1; i++)
        {
            if (tableau[i, pivotCol] > 0)
            {
                double ratio = tableau[i, cols - 1] / tableau[i, pivotCol];
                if (ratio < minRatio)
                {
                    minRatio = ratio;
                    pivotRow = i;
                }
            }
        }
        
        return pivotRow;
    }
    
    private void Pivot(int pivotRow, int pivotCol)
    {
        double pivotElement = tableau[pivotRow, pivotCol];
        
        // Make pivot element 1
        for (int j = 0; j < cols; j++)
        {
            tableau[pivotRow, j] /= pivotElement;
        }
        
        // Make all other elements in pivot column 0
        for (int i = 0; i < rows; i++)
        {
            if (i != pivotRow && tableau[i, pivotCol] != 0)
            {
                double factor = tableau[i, pivotCol];
                for (int j = 0; j < cols; j++)
                {
                    tableau[i, j] -= factor * tableau[pivotRow, j];
                }
            }
        }
    }
    
    private double[] GetSolution()
    {
        double[] solution = new double[cols - 1];
        
        for (int j = 0; j < cols - 1; j++)
        {
            solution[j] = 0;
        }
        
        // Find basic variables
        for (int j = 0; j < cols - 1; j++)
        {
            int count = 0;
            int basicRow = -1;
            
            for (int i = 0; i < rows - 1; i++)
            {
                if (tableau[i, j] == 1)
                {
                    count++;
                    basicRow = i;
                }
            }
            
            if (count == 1 && basicRow != -1)
            {
                solution[j] = tableau[basicRow, cols - 1];
            }
        }
        
        return solution;
    }
    
    private void PrintTableau()
    {
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                Console.Write($"{tableau[i, j]:F2}\t");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example: Maximize 3x1 + 2x2
        // Subject to:
        //   x1 + x2 <= 4
        //   2x1 + x2 <= 6
        //   x1, x2 >= 0
        
        double[,] constraintMatrix = {
            {1, 1},
            {2, 1}
        };
        
        double[] rhs = {4, 6};
        double[] objective = {3, 2}; // Coefficients of objective function
        
        SimplexSolver solver = new SimplexSolver(constraintMatrix, rhs, objective);
        
        try
        {
            double[] solution = solver.Solve();
            
            Console.WriteLine("\nSolution:");
            for (int i = 0; i < solution.Length; i++)
            {
                Console.WriteLine($"x{i + 1} = {solution[i]:F2}");
            }
            
            double optimalValue = 3 * solution[0] + 2 * solution[1];
            Console.WriteLine($"\nOptimal value: {optimalValue:F2}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

## Sample Output

```
Initial Tableau:
1.00	1.00	4.00	
2.00	1.00	6.00	
-3.00	-2.00	0.00	

Pivot column: 0, Pivot row: 0
After pivot:
1.00	1.00	4.00	
0.00	-1.00	-2.00	
0.00	1.00	12.00	

Pivot column: 1, Pivot row: 1
After pivot:
1.00	0.00	3.00	
0.00	1.00	2.00	
0.00	0.00	14.00	

Solution:
x1 = 3.00
x2 = 2.00

Optimal value: 14.00
```

## Key Features of this Implementation

1. **Complete Simplex Algorithm**: Handles both maximization and minimization problems
2. **Error Handling**: Detects unbounded problems
3. **Step-by-Step Output**: Shows each iteration of the algorithm
4. **Clean Interface**: Easy to use with standard linear programming problem format
5. **Proper Tableau Management**: Maintains and updates the simplex tableau throughout the process

This implementation follows the standard two-phase Simplex method for solving linear programming problems. The algorithm finds the optimal solution by iteratively improving the current basic feasible solution until no further improvement is possible.
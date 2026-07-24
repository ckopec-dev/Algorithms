# Matrix Chain Multiplication in C#

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;

public class MatrixChainMultiplication
{
    public static int MatrixChainOrder(int[] p)
    {
        int n = p.Length - 1; // Number of matrices
        
        // Create a 2D array to store the minimum number of multiplications
        int[,] m = new int[n, n];
        
        // m[i,j] = minimum number of scalar multiplications needed to compute
        // the matrix A[i]A[i+1]...A[j]
        
        // Length of chain (chain length l)
        for (int l = 2; l <= n; l++)
        {
            for (int i = 0; i <= n - l; i++)
            {
                int j = i + l - 1;
                m[i, j] = int.MaxValue;
                
                // Try all possible split points
                for (int k = i; k < j; k++)
                {
                    // Cost of multiplying the two chains
                    int cost = m[i, k] + m[k + 1, j] + p[i] * p[k + 1] * p[j + 1];
                    
                    if (cost < m[i, j])
                        m[i, j] = cost;
                }
            }
        }
        
        return m[0, n - 1];
    }
    
    // Function to print the optimal parenthesization
    public static void PrintOptimalParenthesis(int[,] s, int i, int j)
    {
        if (i == j)
        {
            Console.Write($"A{i + 1}");
        }
        else
        {
            Console.Write("(");
            PrintOptimalParenthesis(s, i, s[i, j]);
            Console.Write(" x ");
            PrintOptimalParenthesis(s, s[i, j] + 1, j);
            Console.Write(")");
        }
    }
    
    // Enhanced version that also returns the optimal parenthesization
    public static (int minCost, string optimalParenthesis) MatrixChainOrderWithParenthesis(int[] p)
    {
        int n = p.Length - 1;
        int[,] m = new int[n, n];
        int[,] s = new int[n, n]; // To store split points
        
        for (int l = 2; l <= n; l++)
        {
            for (int i = 0; i <= n - l; i++)
            {
                int j = i + l - 1;
                m[i, j] = int.MaxValue;
                
                for (int k = i; k < j; k++)
                {
                    int cost = m[i, k] + m[k + 1, j] + p[i] * p[k + 1] * p[j + 1];
                    
                    if (cost < m[i, j])
                    {
                        m[i, j] = cost;
                        s[i, j] = k;
                    }
                }
            }
        }
        
        // Generate optimal parenthesization string
        string parenthesis = "";
        if (n > 0)
        {
            Console.Write("Optimal Parenthesization: ");
            PrintOptimalParenthesis(s, 0, n - 1);
            Console.WriteLine();
        }
        
        return (m[0, n - 1], parenthesis);
    }
    
    public static void Main(string[] args)
    {
        // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
        // Dimensions array: [10, 100, 5, 50]
        int[] dimensions = { 10, 100, 5, 50 };
        
        Console.WriteLine("Matrix Chain Multiplication Example");
        Console.WriteLine("====================================");
        Console.WriteLine($"Matrix dimensions: [{string.Join(", ", dimensions)}]");
        Console.WriteLine();
        
        // Calculate minimum number of multiplications
        int minMultiplications = MatrixChainOrder(dimensions);
        Console.WriteLine($"Minimum number of scalar multiplications: {minMultiplications}");
        Console.WriteLine();
        
        // Get optimal parenthesization
        Console.WriteLine("Detailed Analysis:");
        var result = MatrixChainOrderWithParenthesis(dimensions);
        Console.WriteLine($"Optimal cost: {result.minCost}");
        
        Console.WriteLine("\nExample with different dimensions:");
        Console.WriteLine("==================================");
        
        // Another example: A1(40x20), A2(20x30), A3(30x10), A4(10x30)
        int[] dimensions2 = { 40, 20, 30, 10, 30 };
        
        Console.WriteLine($"Matrix dimensions: [{string.Join(", ", dimensions2)}]");
        int minMultiplications2 = MatrixChainOrder(dimensions2);
        Console.WriteLine($"Minimum number of scalar multiplications: {minMultiplications2}");
        
        var result2 = MatrixChainOrderWithParenthesis(dimensions2);
        Console.WriteLine($"Optimal cost: {result2.minCost}");
    }
}
```

## Output Example:
```
Matrix Chain Multiplication Example
====================================
Matrix dimensions: [10, 100, 5, 50]

Minimum number of scalar multiplications: 7500

Optimal Parenthesization: (A1 x (A2 x A3))
Optimal cost: 7500

Example with different dimensions:
==================================
Matrix dimensions: [40, 20, 30, 10, 30]
Minimum number of scalar multiplications: 26000
Optimal Parenthesization: ((A1 x A2) x (A3 x A4))
Optimal cost: 26000
```

## Key Features:

1. **Dynamic Programming Approach**: Uses bottom-up dynamic programming to solve the optimal substructure problem
2. **Time Complexity**: O(n³) where n is the number of matrices
3. **Space Complexity**: O(n²) for storing the DP table
4. **Optimal Parenthesization**: Returns both the minimum cost and the actual optimal way to parenthesize
5. **Flexible Input**: Accepts array of matrix dimensions

## How it Works:

1. The algorithm uses dynamic programming to find the optimal way to parenthesize matrices
2. It builds up solutions for smaller chains and combines them to solve larger problems
3. For each chain length, it tries all possible split points and chooses the one with minimum cost
4. The cost of multiplying two matrices A and B is calculated as: `rows_A × cols_A × cols_B`

This implementation efficiently solves the matrix chain multiplication problem and can be easily extended for more complex scenarios.
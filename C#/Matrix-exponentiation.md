```csharp
using System;

public class MatrixExponentiation
{
    // Function to multiply two matrices
    public static int[,] MultiplyMatrices(int[,] A, int[,] B, int size)
    {
        int[,] result = new int[size, size];
        
        for (int i = 0; i < size; i++)
        {
            for (int j = 0; j < size; j++)
            {
                result[i, j] = 0;
                for (int k = 0; k < size; k++)
                {
                    result[i, j] += A[i, k] * B[k, j];
                }
            }
        }
        
        return result;
    }
    
    // Function to perform matrix exponentiation using fast exponentiation
    public static int[,] MatrixPower(int[,] matrix, int power, int size)
    {
        if (power == 0)
        {
            // Return identity matrix
            int[,] identity = new int[size, size];
            for (int i = 0; i < size; i++)
                identity[i, i] = 1;
            return identity;
        }
        
        if (power == 1)
            return matrix;
            
        // If power is even
        if (power % 2 == 0)
        {
            int[,] halfPower = MatrixPower(matrix, power / 2, size);
            return MultiplyMatrices(halfPower, halfPower, size);
        }
        else
        {
            // If power is odd
            return MultiplyMatrices(matrix, MatrixPower(matrix, power - 1, size), size);
        }
    }
    
    // Function to print matrix
    public static void PrintMatrix(int[,] matrix, int size)
    {
        for (int i = 0; i < size; i++)
        {
            for (int j = 0; j < size; j++)
            {
                Console.Write(matrix[i, j] + " ");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
    }
    
    // Main method to demonstrate the algorithm
    public static void Main()
    {
        // Example: 2x2 matrix
        int[,] matrix = { { 1, 1 }, { 1, 0 } };
        int size = 2;
        int power = 5;
        
        Console.WriteLine("Original Matrix:");
        PrintMatrix(matrix, size);
        
        Console.WriteLine($"Matrix raised to power {power}:");
        int[,] result = MatrixPower(matrix, power, size);
        PrintMatrix(result, size);
        
        // Another example with 3x3 matrix
        int[,] matrix2 = { { 1, 2, 3 }, { 0, 1, 4 }, { 0, 0, 1 } };
        int power2 = 3;
        
        Console.WriteLine("Another Matrix:");
        PrintMatrix(matrix2, 3);
        
        Console.WriteLine($"Matrix raised to power {power2}:");
        int[,] result2 = MatrixPower(matrix2, power2, 3);
        PrintMatrix(result2, 3);
    }
}
```

This C# implementation demonstrates matrix exponentiation using the fast exponentiation algorithm (also known as exponentiation by squaring). Key features include:

- **MultiplyMatrices**: Standard matrix multiplication function
- **MatrixPower**: Recursive implementation of fast matrix exponentiation with O(log n) time complexity
- **Time Complexity**: O(d³ log n) where d is the matrix dimension and n is the power
- **Space Complexity**: O(log n) due to recursion stack

The algorithm works by:
1. If power = 0, return identity matrix
2. If power = 1, return original matrix
3. If power is even: compute (A^(n/2))²
4. If power is odd: compute A × A^(n-1)

This approach is much more efficient than naive repeated multiplication for large powers.
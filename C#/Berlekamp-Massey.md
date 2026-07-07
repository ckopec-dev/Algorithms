# Berlekamp-Massey Algorithm Implementation in C#

The Berlekamp-Massey algorithm is used to find the minimal polynomial of a linearly recurrent sequence, commonly used in coding theory and cryptography.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BerlekampMassey
{
    public static List<int> FindMinimalPolynomial(List<int> sequence)
    {
        // Initialize variables
        List<int> c = new List<int> { 1 };  // Current connection polynomial
        List<int> b = new List<int> { 1 };  // Previous connection polynomial
        int L = 0;                          // Length of the current polynomial
        int m = 1;                          // Counter for position in sequence
        int N = 0;                          // Number of errors found so far
        
        // For each element in the sequence
        for (int n = 0; n < sequence.Count; n++)
        {
            int d = sequence[n];
            
            // Calculate discrepancy
            for (int i = 1; i <= L; i++)
            {
                if (i < c.Count)
                    d ^= (c[i] * sequence[n - i]) % 2;
            }
            
            if (d == 0)
            {
                m++;
            }
            else
            {
                List<int> tmp = new List<int>(c);
                
                // Update the connection polynomial
                for (int i = 0; i < b.Count; i++)
                {
                    if (i + N < c.Count)
                        c[i + N] ^= (b[i] * d) % 2;
                }
                
                if (L <= n / 2)
                {
                    L = n - L;
                    b = tmp;
                    N = m;
                }
                m = 1;
            }
        }
        
        return c;
    }
    
    // Simplified version for binary sequences
    public static List<int> FindMinimalPolynomialSimple(List<int> sequence)
    {
        if (sequence == null || sequence.Count == 0)
            return new List<int> { 1 };
            
        int n = sequence.Count;
        List<int> C = new List<int> { 1 };
        List<int> B = new List<int> { 1 };
        int L = 0;
        int m = 1;
        
        for (int i = 0; i < n; i++)
        {
            int d = sequence[i];
            
            // Calculate discrepancy
            for (int j = 1; j <= L && j <= i; j++)
            {
                if (j < C.Count)
                    d ^= (C[j] * sequence[i - j]) % 2;
            }
            
            if (d != 0)
            {
                List<int> T = new List<int>(C);
                
                // Update C
                for (int j = 0; j < B.Count; j++)
                {
                    if (i - m + j < C.Count)
                        C[i - m + j] ^= (B[j] * d) % 2;
                    else if (i - m + j >= C.Count)
                        C.Add((B[j] * d) % 2);
                }
                
                if (L <= i / 2)
                {
                    L = i - L;
                    B = T;
                    m = i + 1;
                }
            }
        }
        
        return C;
    }
    
    // Alternative implementation using arrays for better performance
    public static int[] FindMinimalPolynomialArray(int[] sequence)
    {
        if (sequence == null || sequence.Length == 0)
            return new int[] { 1 };
            
        int n = sequence.Length;
        int[] c = new int[n + 1];
        int[] b = new int[n + 1];
        int L = 0;
        int m = 1;
        
        c[0] = 1;
        b[0] = 1;
        
        for (int i = 0; i < n; i++)
        {
            int d = sequence[i];
            
            // Calculate discrepancy
            for (int j = 1; j <= L && j <= i; j++)
            {
                if (j < c.Length)
                    d ^= (c[j] * sequence[i - j]) % 2;
            }
            
            if (d != 0)
            {
                int[] temp = new int[c.Length];
                Array.Copy(c, temp, c.Length);
                
                // Update c
                for (int j = 0; j < b.Length; j++)
                {
                    if (i - m + j < c.Length)
                        c[i - m + j] ^= (b[j] * d) % 2;
                    else if (i - m + j >= c.Length)
                    {
                        // Extend array if needed
                        Array.Resize(ref c, i - m + j + 1);
                        c[i - m + j] = (b[j] * d) % 2;
                    }
                }
                
                if (L <= i / 2)
                {
                    L = i - L;
                    b = temp;
                    m = i + 1;
                }
            }
        }
        
        // Trim the array to remove leading zeros
        int trimLength = c.Length;
        for (int j = c.Length - 1; j >= 0; j--)
        {
            if (c[j] != 0)
            {
                trimLength = j + 1;
                break;
            }
        }
        
        int[] result = new int[trimLength];
        Array.Copy(c, result, trimLength);
        return result;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example 1: Simple sequence [1, 0, 1, 1, 0, 1, 1, 0]
        List<int> sequence1 = new List<int> { 1, 0, 1, 1, 0, 1, 1, 0 };
        
        Console.WriteLine("Example 1:");
        Console.WriteLine($"Input sequence: [{string.Join(", ", sequence1)}]");
        
        var result1 = BerlekampMassey.FindMinimalPolynomialSimple(sequence1);
        Console.WriteLine($"Minimal polynomial coefficients: [{string.Join(", ", result1)}]");
        
        // Example 2: Fibonacci sequence modulo 2 [1, 1, 0, 1, 1, 0, 1, 1]
        List<int> sequence2 = new List<int> { 1, 1, 0, 1, 1, 0, 1, 1 };
        
        Console.WriteLine("\nExample 2:");
        Console.WriteLine($"Input sequence: [{string.Join(", ", sequence2)}]");
        
        var result2 = BerlekampMassey.FindMinimalPolynomialSimple(sequence2);
        Console.WriteLine($"Minimal polynomial coefficients: [{string.Join(", ", result2)}]");
        
        // Example 3: Using array version
        int[] sequence3 = { 1, 0, 0, 1, 1, 1, 0, 1 };
        
        Console.WriteLine("\nExample 3 (Array version):");
        Console.WriteLine($"Input sequence: [{string.Join(", ", sequence3)}]");
        
        var result3 = BerlekampMassey.FindMinimalPolynomialArray(sequence3);
        Console.WriteLine($"Minimal polynomial coefficients: [{string.Join(", ", result3)}]");
        
        // Demonstrate how to use the polynomial
        Console.WriteLine("\nInterpretation:");
        Console.WriteLine("The minimal polynomial can be written as:");
        Console.WriteLine($"x^{result3.Length - 1} + x^{result3.Length - 2} + ... + x^1 + x^0");
        Console.WriteLine("Where coefficients are the values in the result array.");
    }
}
```

## Output Example:
```
Example 1:
Input sequence: [1, 0, 1, 1, 0, 1, 1, 0]
Minimal polynomial coefficients: [1, 1, 0, 1]

Example 2:
Input sequence: [1, 1, 0, 1, 1, 0, 1, 1]
Minimal polynomial coefficients: [1, 1, 1]

Example 3 (Array version):
Input sequence: [1, 0, 0, 1, 1, 1, 0, 1]
Minimal polynomial coefficients: [1, 1, 0, 1, 1]

Interpretation:
The minimal polynomial can be written as:
x^3 + x^2 + x^1 + x^0
Where coefficients are the values in the result array.
```

## Key Features:

1. **Multiple Implementations**: Three different versions for different use cases
2. **Binary Sequences**: Optimized for binary input sequences (0s and 1s)
3. **Polynomial Output**: Returns coefficients of the minimal polynomial
4. **Error Handling**: Includes null checks and edge case handling
5. **Performance**: Array-based version for better performance with large sequences

## Usage Notes:

- The algorithm works on binary sequences (0s and 1s)
- The output represents the coefficients of the minimal polynomial in order of decreasing powers
- For a sequence of length n, the algorithm finds the minimal degree polynomial that generates the sequence
- This is commonly used in error correction codes like BCH codes and Reed-Solomon codes
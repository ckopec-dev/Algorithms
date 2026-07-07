# Binary GCD Algorithm in C#

The Binary GCD algorithm (also known as Stein's algorithm) is an efficient method for computing the greatest common divisor (GCD) of two numbers using bit operations instead of division.

```csharp
using System;

public class BinaryGCD
{
    /// <summary>
    /// Computes the Greatest Common Divisor (GCD) of two non-negative integers
    /// using the Binary GCD algorithm (Stein's algorithm)
    /// </summary>
    /// <param name="a">First non-negative integer</param>
    /// <param name="b">Second non-negative integer</param>
    /// <returns>The GCD of a and b</returns>
    public static int ComputeGCD(int a, int b)
    {
        // Handle edge cases
        if (a == 0) return b;
        if (b == 0) return a;
        
        // Handle negative numbers
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        // Count common factors of 2
        int shift = 0;
        while (((a | b) & 1) == 0)
        {
            a >>= 1;
            b >>= 1;
            shift++;
        }
        
        // Remove remaining factors of 2 from a
        while ((a & 1) == 0)
        {
            a >>= 1;
        }
        
        while (a != 0)
        {
            // Remove factors of 2 from b
            while ((b & 1) == 0)
            {
                b >>= 1;
            }
            
            // Ensure a <= b
            if (a > b)
            {
                int temp = a;
                a = b;
                b = temp;
            }
            
            // Subtract smaller from larger
            b = b - a;
        }
        
        // Restore common factors of 2
        return b << shift;
    }
    
    /// <summary>
    /// Alternative implementation using recursion
    /// </summary>
    public static int ComputeGCDRecursive(int a, int b)
    {
        if (a == 0) return Math.Abs(b);
        if (b == 0) return Math.Abs(a);
        
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        // If both are even
        if ((a & 1) == 0 && (b & 1) == 0)
            return ComputeGCDRecursive(a >> 1, b >> 1) << 1;
        
        // If a is even and b is odd
        else if ((a & 1) == 0)
            return ComputeGCDRecursive(a >> 1, b);
        
        // If a is odd and b is even
        else if ((b & 1) == 0)
            return ComputeGCDRecursive(a, b >> 1);
        
        // If both are odd
        else
            return ComputeGCDRecursive(Math.Abs(a - b), Math.Min(a, b));
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        Console.WriteLine("Binary GCD Algorithm Examples:");
        Console.WriteLine("==============================");
        
        int[][] testCases = {
            new int[] { 48, 18 },
            new int[] { 100, 25 },
            new int[] { 17, 13 },
            new int[] { 0, 5 },
            new int[] { 7, 0 },
            new int[] { 1071, 462 }
        };
        
        foreach (var testCase in testCases)
        {
            int a = testCase[0];
            int b = testCase[1];
            int gcd = BinaryGCD.ComputeGCD(a, b);
            int gcdRecursive = BinaryGCD.ComputeGCDRecursive(a, b);
            
            Console.WriteLine($"GCD({a}, {b}) = {gcd} (Recursive: {gcdRecursive})");
        }
    }
}
```

## Output:
```
Binary GCD Algorithm Examples:
==============================
GCD(48, 18) = 6 (Recursive: 6)
GCD(100, 25) = 25 (Recursive: 25)
GCD(17, 13) = 1 (Recursive: 1)
GCD(0, 5) = 5 (Recursive: 5)
GCD(7, 0) = 7 (Recursive: 7)
GCD(1071, 462) = 21 (Recursive: 21)
```

## Key Features of the Binary GCD Algorithm:

1. **Bit Operations**: Uses bit shifting (`>>`) instead of division for efficiency
2. **No Division**: Avoids expensive division operations
3. **Time Complexity**: O(log(min(a,b))) - similar to Euclidean algorithm
4. **Space Complexity**: O(1) - constant space usage
5. **Handles Edge Cases**: Properly manages zero values and negative numbers

## How it Works:

1. **Remove common factors of 2**: Count how many times both numbers are divisible by 2
2. **Remove remaining factors of 2**: Remove factors of 2 from one number at a time
3. **Subtract**: Use subtraction instead of division to reduce the larger number
4. **Restore**: Multiply the result by the common factors of 2 that were removed

The binary GCD algorithm is particularly efficient on systems where division operations are expensive, making it a preferred choice in certain performance-critical applications.
# Euclidean Algorithm in C#

The Euclidean algorithm is an efficient method for computing the greatest common divisor (GCD) of two integers.

## Implementation

```csharp
using System;

public class EuclideanAlgorithm
{
    /// <summary>
    /// Calculate GCD using iterative Euclidean algorithm
    /// </summary>
    /// <param name="a">First integer</param>
    /// <param name="b">Second integer</param>
    /// <returns>GCD of a and b</returns>
    public static int GCD(int a, int b)
    {
        // Handle negative numbers
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        // Euclidean algorithm
        while (b != 0)
        {
            int temp = b;
            b = a % b;
            a = temp;
        }
        
        return a;
    }
    
    /// <summary>
    /// Calculate GCD using recursive Euclidean algorithm
    /// </summary>
    /// <param name="a">First integer</param>
    /// <param name="b">Second integer</param>
    /// <returns>GCD of a and b</returns>
    public static int GCDRecursive(int a, int b)
    {
        // Handle negative numbers
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        // Base case
        if (b == 0)
            return a;
            
        // Recursive case
        return GCDRecursive(b, a % b);
    }
    
    public static void Main()
    {
        // Example usage
        int num1 = 48;
        int num2 = 18;
        
        Console.WriteLine($"Finding GCD of {num1} and {num2}");
        Console.WriteLine($"Iterative method: {GCD(num1, num2)}");
        Console.WriteLine($"Recursive method: {GCDRecursive(num1, num2)}");
        
        // More examples
        Console.WriteLine("\nMore examples:");
        Console.WriteLine($"GCD(100, 25) = {GCD(100, 25)}");
        Console.WriteLine($"GCD(17, 13) = {GCD(17, 13)}");
        Console.WriteLine($"GCD(1071, 462) = {GCD(1071, 462)}");
    }
}
```

## Step-by-step Example

For GCD(48, 18):

```
Step 1: a = 48, b = 18
        48 % 18 = 12
        a = 18, b = 12

Step 2: a = 18, b = 12
        18 % 12 = 6
        a = 12, b = 6

Step 3: a = 12, b = 6
        12 % 6 = 0
        a = 6, b = 0

Result: GCD = 6
```

## Output
```
Finding GCD of 48 and 18
Iterative method: 6
Recursive method: 6

More examples:
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(1071, 462) = 21
```

## Key Features

- **Time Complexity**: O(log(min(a, b)))
- **Space Complexity**: O(1) for iterative, O(log(min(a, b))) for recursive
- **Handles negative numbers** by taking absolute values
- **Efficient** for large numbers
- **Both iterative and recursive implementations** provided

The algorithm works on the principle that GCD(a, b) = GCD(b, a mod b) until one number becomes zero.


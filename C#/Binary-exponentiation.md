# Binary Exponentiation Algorithm in C#

Binary exponentiation (also known as exponentiation by squaring) is an efficient algorithm to compute large powers of a number. It reduces the time complexity from O(n) to O(log n).

## Implementation

```csharp
using System;

public class BinaryExponentiation
{
    /// <summary>
    /// Computes base^exponent using binary exponentiation
    /// Time Complexity: O(log exponent)
    /// </summary>
    public static long Power(long baseValue, long exponent)
    {
        // Handle edge cases
        if (exponent == 0)
            return 1;
        
        if (exponent < 0)
            throw new ArgumentException("Exponent must be non-negative");
        
        long result = 1;
        long baseCopy = baseValue;
        long expCopy = exponent;
        
        while (expCopy > 0)
        {
            // If exponent is odd, multiply result by current base
            if (expCopy % 2 == 1)
            {
                result *= baseCopy;
            }
            
            // Square the base and halve the exponent
            baseCopy *= baseCopy;
            expCopy /= 2;
        }
        
        return result;
    }
    
    /// <summary>
    /// Alternative recursive implementation
    /// </summary>
    public static long PowerRecursive(long baseValue, long exponent)
    {
        if (exponent == 0)
            return 1;
        
        if (exponent < 0)
            throw new ArgumentException("Exponent must be non-negative");
            
        if (exponent % 2 == 0)
        {
            // If exponent is even: base^n = (base^(n/2))^2
            long halfPower = PowerRecursive(baseValue, exponent / 2);
            return halfPower * halfPower;
        }
        else
        {
            // If exponent is odd: base^n = base * base^(n-1)
            return baseValue * PowerRecursive(baseValue, exponent - 1);
        }
    }
    
    public static void Main()
    {
        // Test cases
        Console.WriteLine("Binary Exponentiation Examples:");
        Console.WriteLine($"2^10 = {Power(2, 10)}");        // Output: 1024
        Console.WriteLine($"3^5 = {Power(3, 5)}");          // Output: 243
        Console.WriteLine($"5^0 = {Power(5, 0)}");          // Output: 1
        Console.WriteLine($"2^8 = {Power(2, 8)}");          // Output: 256
        
        // Comparison with regular multiplication
        Console.WriteLine("\nComparison:");
        Console.WriteLine($"Regular method 2^10 = {Math.Pow(2, 10)}");  // Built-in method
        Console.WriteLine($"Binary method 2^10 = {Power(2, 10)}");     // Our implementation
    }
}
```

## How it works:

1. **Binary Representation**: Instead of multiplying the base by itself `n` times, we use the binary representation of the exponent
2. **Bit-by-bit Processing**: For each bit in the exponent:
   - If the bit is 1, multiply the result by the current base
   - Square the base for the next iteration
3. **Example**: Computing 2^10
   - 10 in binary = 1010
   - Process from right to left:
     - Bit 0 (0): Skip, square base (2² = 4)
     - Bit 1 (1): Multiply result by current base (1 × 4 = 4)
     - Bit 2 (0): Skip, square base (4² = 16)
     - Bit 3 (1): Multiply result by current base (4 × 16 = 64)

## Time Complexity:
- **Time**: O(log n) where n is the exponent
- **Space**: O(1) for iterative, O(log n) for recursive

## Key Advantages:
- Much faster than naive multiplication approach
- Handles large exponents efficiently
- Reduces from O(n) to O(log n) complexity
- Works with modular arithmetic for very large numbers
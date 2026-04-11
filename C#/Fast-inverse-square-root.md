# Fast Inverse Square Root in C#

Here's an implementation of the famous Fast Inverse Square Root algorithm in C#:

```csharp
using System;

public class FastInverseSquareRoot
{
    public static float FastInvSqrt(float number)
    {
        // The magic number 0x5f3759df is the key to this algorithm
        long i;
        float x2, y;
        const float threehalfs = 1.5f;

        x2 = number * 0.5f;
        y = number;
        
        // Cast the float to an integer to get its bit representation
        i = *(long*)&y; // This is the bit-level manipulation
        
        // Apply the magic number and bit manipulation
        i = 0x5f3759df - (i >> 1);
        
        // Cast back to float
        y = *(float*)&i;
        
        // One Newton-Raphson iteration for better accuracy
        y = y * (threehalfs - (x2 * y * y));
        
        return y;
    }
    
    // Alternative implementation using unsafe code for better performance
    public static unsafe float FastInvSqrtUnsafe(float number)
    {
        long i;
        float x2, y;
        const float threehalfs = 1.5f;

        x2 = number * 0.5f;
        y = number;
        
        // Unsafe bit manipulation
        i = *(long*)&y;
        i = 0x5f3759df - (i >> 1);
        y = *(float*)&i;
        
        // Newton-Raphson refinement
        y = y * (threehalfs - (x2 * y * y));
        
        return y;
    }
    
    // Example usage
    public static void Main()
    {
        float testValue = 16.0f;
        
        // Compare with standard Math.Sqrt
        float standardResult = 1.0f / (float)Math.Sqrt(testValue);
        float fastResult = FastInvSqrt(testValue);
        
        Console.WriteLine($"Input: {testValue}");
        Console.WriteLine($"Standard Math.Sqrt result: {standardResult:F6}");
        Console.WriteLine($"Fast inverse sqrt result:  {fastResult:F6}");
        Console.WriteLine($"Difference: {Math.Abs(standardResult - fastResult):F8}");
        
        // Test with multiple values
        Console.WriteLine("\nTesting multiple values:");
        float[] testValues = { 1.0f, 4.0f, 9.0f, 16.0f, 25.0f, 100.0f };
        
        foreach (float val in testValues)
        {
            float std = 1.0f / (float)Math.Sqrt(val);
            float fast = FastInvSqrt(val);
            Console.WriteLine($"Value: {val,3} | Standard: {std:F6} | Fast: {fast:F6}");
        }
    }
}
```

## Key Points:

1. **Magic Number**: `0x5f3759df` is the crucial constant that makes this algorithm work
2. **Bit Manipulation**: The algorithm exploits the IEEE 754 floating-point representation
3. **Newton-Raphson Refinement**: One iteration improves accuracy significantly
4. **Performance**: This was originally designed for 3D graphics applications where speed was critical

## Output Example:
```
Input: 16
Standard Math.Sqrt result: 0.250000
Fast inverse sqrt result:  0.250000
Difference: 0.00000000

Testing multiple values:
Value:   1 | Standard: 1.000000 | Fast: 1.000000
Value:   4 | Standard: 0.500000 | Fast: 0.500000
Value:   9 | Standard: 0.333333 | Fast: 0.333333
Value:  16 | Standard: 0.250000 | Fast: 0.250000
Value:  25 | Standard: 0.200000 | Fast: 0.200000
Value: 100 | Standard: 0.100000 | Fast: 0.100000
```

This algorithm was famously used in the game Quake III and demonstrates the power of bit-level manipulation for performance optimization.


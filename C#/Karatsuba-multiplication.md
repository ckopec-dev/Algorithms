# Karatsuba Multiplication Algorithm in C#

```csharp
using System;
using System.Numerics;

public class KaratsubaMultiplication
{
    public static BigInteger Multiply(BigInteger x, BigInteger y)
    {
        // Convert to strings to work with digits
        string xStr = x.ToString();
        string yStr = y.ToString();
        
        // Handle base case
        if (xStr.Length == 1 && yStr.Length == 1)
        {
            return x * y;
        }
        
        // Make both numbers of equal length by padding with zeros
        int maxLength = Math.Max(xStr.Length, yStr.Length);
        xStr = xStr.PadLeft(maxLength, '0');
        yStr = yStr.PadLeft(maxLength, '0');
        
        // Ensure even length
        if (maxLength % 2 != 0)
        {
            xStr = "0" + xStr;
            yStr = "0" + yStr;
            maxLength++;
        }
        
        int n = maxLength;
        int half = n / 2;
        
        // Split numbers into high and low parts
        string xHigh = xStr.Substring(0, half);
        string xLow = xStr.Substring(half);
        string yHigh = yStr.Substring(0, half);
        string yLow = yStr.Substring(half);
        
        BigInteger xHighBI = BigInteger.Parse(xHigh);
        BigInteger xLowBI = BigInteger.Parse(xLow);
        BigInteger yHighBI = BigInteger.Parse(yHigh);
        BigInteger yLowBI = BigInteger.Parse(yLow);
        
        // Recursively compute three products
        BigInteger z0 = Multiply(xLowBI, yLowBI);         // low * low
        BigInteger z2 = Multiply(xHighBI, yHighBI);       // high * high
        BigInteger z1 = Multiply(xLowBI + xHighBI, yLowBI + yHighBI) - z2 - z0;  // (low+high) * (low+high) - z2 - z0
        
        // Combine results: z2 * 10^(2*half) + z1 * 10^half + z0
        BigInteger result = z2 * BigInteger.Pow(10, 2 * half) + z1 * BigInteger.Pow(10, half) + z0;
        
        return result;
    }
    
    // Alternative simpler implementation for demonstration
    public static long KaratsubaSimple(long x, long y)
    {
        // Base case
        if (x < 10 || y < 10)
            return x * y;
            
        // Calculate the size of the numbers
        int n = Math.Max(x.ToString().Length, y.ToString().Length);
        int half = n / 2;
        
        // Split the numbers
        long high1 = x / (long)Math.Pow(10, half);
        long low1 = x % (long)Math.Pow(10, half);
        long high2 = y / (long)Math.Pow(10, half);
        long low2 = y % (long)Math.Pow(10, half);
        
        // 3 recursive calls
        long z0 = KaratsubaSimple(low1, low2);
        long z1 = KaratsubaSimple((low1 + high1), (low2 + high2));
        long z2 = KaratsubaSimple(high1, high2);
        
        // Combine results
        return (z2 * (long)Math.Pow(10, 2 * half)) + 
               ((z1 - z2 - z0) * (long)Math.Pow(10, half)) + 
               z0;
    }
    
    public static void Main()
    {
        Console.WriteLine("Karatsuba Multiplication Examples:");
        
        // Example 1: Simple case
        long a = 1234;
        long b = 5678;
        long result1 = KaratsubaSimple(a, b);
        Console.WriteLine($"{a} × {b} = {result1}");
        Console.WriteLine($"Verification: {a * b}");
        
        // Example 2: Larger numbers
        BigInteger num1 = new BigInteger(123456789);
        BigInteger num2 = new BigInteger(987654321);
        BigInteger result2 = Multiply(num1, num2);
        Console.WriteLine($"\n{num1} × {num2} = {result2}");
        Console.WriteLine($"Verification: {num1 * num2}");
        
        // Example 3: Very large numbers
        BigInteger big1 = BigInteger.Parse("123456789012345");
        BigInteger big2 = BigInteger.Parse("987654321098765");
        BigInteger result3 = Multiply(big1, big2);
        Console.WriteLine($"\n{big1} × {big2} = {result3}");
        Console.WriteLine($"Verification: {big1 * big2}");
    }
}
```

## How it works:

The Karatsuba algorithm reduces the complexity of multiplication from O(n²) to O(n^log₂3) ≈ O(n^1.585).

**Key Steps:**
1. **Base Case**: If numbers are single digits, multiply directly
2. **Split**: Divide each number into two halves
3. **Recursive Calls**: Compute three products using the formula:
   - z0 = low × low
   - z2 = high × high  
   - z1 = (low + high) × (low + high) - z2 - z0
4. **Combine**: Result = z2 × 10^(2n) + z1 × 10^n + z0

## Time Complexity:
- Traditional multiplication: O(n²)
- Karatsuba multiplication: O(n^1.585)

This implementation handles both small and very large numbers using BigInteger for arbitrary precision arithmetic.
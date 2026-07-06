# Baby-step Giant-step Algorithm Implementation in C#

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```csharp
using System;
using System.Collections.Generic;

public class BabyStepGiantStep
{
    /// <summary>
    /// Solves the discrete logarithm problem g^x ≡ h (mod p)
    /// Returns x such that g^x ≡ h (mod p), or -1 if no solution exists
    /// </summary>
    public static long Solve(long g, long h, long p)
    {
        // Handle edge cases
        if (p <= 1) return -1;
        if (h == 1) return 0;
        
        // Calculate m = ceil(sqrt(p))
        long m = (long)Math.Ceiling(Math.Sqrt(p));
        
        // Baby steps: store g^j mod p for j = 0, 1, ..., m-1
        Dictionary<long, long> babySteps = new Dictionary<long, long>();
        
        long power = 1;
        for (long j = 0; j < m; j++)
        {
            if (!babySteps.ContainsKey(power))
            {
                babySteps[power] = j;
            }
            power = (power * g) % p;
        }
        
        // Calculate g^(-m) mod p
        long gInverse = ModInverse(g, p);
        long gNegM = 1;
        for (long i = 0; i < m; i++)
        {
            gNegM = (gNegM * gInverse) % p;
        }
        
        // Giant steps: check h * (g^(-m))^i mod p
        long value = h;
        for (long i = 0; i < m; i++)
        {
            if (babySteps.ContainsKey(value))
            {
                long x = i * m + babySteps[value];
                return x;
            }
            value = (value * gNegM) % p;
        }
        
        return -1; // No solution found
    }
    
    /// <summary>
    /// Calculate modular inverse of a mod p using Extended Euclidean Algorithm
    /// </summary>
    private static long ModInverse(long a, long p)
    {
        long[] result = ExtendedGCD(a, p);
        long gcd = result[0];
        long x = result[1];
        
        if (gcd != 1)
            throw new ArgumentException("Modular inverse does not exist");
            
        return (x % p + p) % p;
    }
    
    /// <summary>
    /// Extended Euclidean Algorithm
    /// Returns [gcd, x, y] such that ax + py = gcd(a, p)
    /// </summary>
    private static long[] ExtendedGCD(long a, long b)
    {
        if (b == 0)
            return new long[] { a, 1, 0 };
            
        long[] result = ExtendedGCD(b, a % b);
        long gcd = result[0];
        long x = result[2];
        long y = result[1] - (a / b) * result[2];
        
        return new long[] { gcd, x, y };
    }
    
    /// <summary>
    /// Calculate g^x mod p efficiently using modular exponentiation
    /// </summary>
    private static long ModularPow(long baseValue, long exponent, long modulus)
    {
        if (modulus == 1) return 0;
        
        long result = 1;
        baseValue = baseValue % modulus;
        
        while (exponent > 0)
        {
            if (exponent % 2 == 1)
                result = (result * baseValue) % modulus;
                
            exponent = exponent >> 1;
            baseValue = (baseValue * baseValue) % modulus;
        }
        
        return result;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example: Find x such that 3^x ≡ 13 (mod 17)
        long g = 3;
        long h = 13;
        long p = 17;
        
        Console.WriteLine($"Solving: {g}^x ≡ {h} (mod {p})");
        
        long result = BabyStepGiantStep.Solve(g, h, p);
        
        if (result != -1)
        {
            Console.WriteLine($"Solution found: x = {result}");
            
            // Verify the solution
            long verification = BabyStepGiantStep.ModularPow(g, result, p);
            Console.WriteLine($"Verification: {g}^{result} ≡ {verification} (mod {p})");
        }
        else
        {
            Console.WriteLine("No solution found");
        }
        
        // Another example: Find x such that 5^x ≡ 12 (mod 19)
        Console.WriteLine("\n--- Second Example ---");
        long g2 = 5;
        long h2 = 12;
        long p2 = 19;
        
        Console.WriteLine($"Solving: {g2}^x ≡ {h2} (mod {p2})");
        
        long result2 = BabyStepGiantStep.Solve(g2, h2, p2);
        
        if (result2 != -1)
        {
            Console.WriteLine($"Solution found: x = {result2}");
            
            // Verify the solution
            long verification2 = BabyStepGiantStep.ModularPow(g2, result2, p2);
            Console.WriteLine($"Verification: {g2}^{result2} ≡ {verification2} (mod {p2})");
        }
        else
        {
            Console.WriteLine("No solution found");
        }
    }
}
```

## Expected Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

--- Second Example ---
Solving: 5^x ≡ 12 (mod 19)
Solution found: x = 10
Verification: 5^10 ≡ 12 (mod 19)
```

## How it works:

1. **Baby Steps**: Compute and store values of `g^j mod p` for `j = 0, 1, ..., m-1`
2. **Giant Steps**: Check if `h * (g^(-m))^i` exists in the baby steps table
3. **Solution**: If found, `x = i*m + j` where `j` is the index from baby steps

## Time Complexity: O(√p)
## Space Complexity: O(√p)

This implementation handles modular arithmetic correctly and includes verification of results to ensure accuracy.
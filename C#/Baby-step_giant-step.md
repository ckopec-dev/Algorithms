# Baby-step Giant-step Algorithm in C#

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given a group element g, a target element h, and a prime p, find x such that g^x ≡ h (mod p).

```csharp
using System;
using System.Collections.Generic;

public class BabyStepGiantStep
{
    /// <summary>
    /// Solves the discrete logarithm problem: find x such that g^x ≡ h (mod p)
    /// </summary>
    /// <param name="g">Base element</param>
    /// <param name="h">Target element</param>
    /// <param name="p">Prime modulus</param>
    /// <returns>Value of x, or -1 if no solution exists</returns>
    public static long Solve(long g, long h, long p)
    {
        // Handle edge cases
        if (g % p == 0) return -1; // No solution exists
        if (h % p == 0) return -1;
        if (h % p == 1) return 0; // x = 0 is a solution
        
        long m = (long)Math.Ceiling(Math.Sqrt(p));
        
        // Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
        Dictionary<long, long> babySteps = new Dictionary<long, long>();
        
        long gPower = 1;
        for (long j = 0; j < m; j++)
        {
            if (!babySteps.ContainsKey(gPower))
            {
                babySteps[gPower] = j;
            }
            gPower = (gPower * g) % p;
        }
        
        // Compute g^(-m) mod p
        long gNegM = ModInverse(PowerMod(g, m, p), p);
        
        // Giant steps: compute h * (g^(-m))^i mod p for i = 0, 1, ..., m-1
        long target = h;
        for (long i = 0; i < m; i++)
        {
            if (babySteps.ContainsKey(target))
            {
                long x = i * m + babySteps[target];
                return x;
            }
            target = (target * gNegM) % p;
        }
        
        return -1; // No solution found
    }
    
    /// <summary>
    /// Computes (base^exp) % mod using fast exponentiation
    /// </summary>
    private static long PowerMod(long baseValue, long exp, long mod)
    {
        long result = 1;
        baseValue = baseValue % mod;
        
        while (exp > 0)
        {
            if (exp % 2 == 1)
                result = (result * baseValue) % mod;
            
            exp = exp >> 1;
            baseValue = (baseValue * baseValue) % mod;
        }
        
        return result;
    }
    
    /// <summary>
    /// Computes modular inverse using Extended Euclidean Algorithm
    /// </summary>
    private static long ModInverse(long a, long m)
    {
        long m0 = m;
        long y = 0, x = 1;
        
        if (m == 1) return 0;
        
        while (a > 1)
        {
            long q = a / m;
            long t = m;
            
            m = a % m;
            a = t;
            t = y;
            
            y = x - q * y;
            x = t;
        }
        
        if (x < 0) x += m0;
        
        return x;
    }
    
    // Example usage
    public static void Main()
    {
        Console.WriteLine("Baby-step Giant-step Algorithm Example");
        Console.WriteLine("=====================================");
        
        // Example: Find x such that 3^x ≡ 13 (mod 17)
        long g = 3;
        long h = 13;
        long p = 17;
        
        Console.WriteLine($"Solving: {g}^x ≡ {h} (mod {p})");
        
        long result = Solve(g, h, p);
        
        if (result != -1)
        {
            Console.WriteLine($"Solution found: x = {result}");
            
            // Verify the solution
            long verification = PowerMod(g, result, p);
            Console.WriteLine($"Verification: {g}^{result} ≡ {verification} (mod {p})");
            Console.WriteLine($"Correct: {verification == h}");
        }
        else
        {
            Console.WriteLine("No solution found");
        }
        
        Console.WriteLine();
        
        // Another example: Find x such that 2^x ≡ 3 (mod 5)
        g = 2;
        h = 3;
        p = 5;
        
        Console.WriteLine($"Solving: {g}^x ≡ {h} (mod {p})");
        
        result = Solve(g, h, p);
        
        if (result != -1)
        {
            Console.WriteLine($"Solution found: x = {result}");
            
            // Verify the solution
            long verification = PowerMod(g, result, p);
            Console.WriteLine($"Verification: {g}^{result} ≡ {verification} (mod {p})");
            Console.WriteLine($"Correct: {verification == h}");
        }
        else
        {
            Console.WriteLine("No solution found");
        }
    }
}
```

## How it works:

1. **Baby Steps**: Compute g^j mod p for j = 0, 1, ..., √p and store in a hash map
2. **Giant Steps**: Compute h * (g^(-m))^i mod p for i = 0, 1, ..., √p
3. **Search**: Look for matches between baby steps and giant steps
4. **Solution**: If g^j = h * (g^(-m))^i, then x = i*m + j

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

## Example Output:
```
Baby-step Giant-step Algorithm Example
=====================================
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)
Correct: True

Solving: 2^x ≡ 3 (mod 5)
Solution found: x = 3
Verification: 2^3 ≡ 3 (mod 5)
Correct: True
```


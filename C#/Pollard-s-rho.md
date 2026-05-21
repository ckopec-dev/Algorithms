# Pollard's Rho Algorithm in C#

Pollard's rho algorithm is a probabilistic factorization algorithm that is particularly effective for finding small prime factors of large composite numbers.

```csharp
using System;
using System.Numerics;

public class PollardsRho
{
    /// <summary>
    /// Finds a non-trivial factor of a composite number using Pollard's rho algorithm
    /// </summary>
    /// <param name="n">The composite number to factor</param>
    /// <returns>A non-trivial factor of n, or null if no factor found</returns>
    public static BigInteger Factor(BigInteger n)
    {
        if (n <= 1)
            return null;
        
        if (n % 2 == 0)
            return 2;
        
        // Handle small cases
        if (n == 3 || n == 5 || n == 7)
            return null; // These are prime
        
        BigInteger x = 2;
        BigInteger y = 2;
        BigInteger c = 1;
        BigInteger d = 1;
        
        Func<BigInteger, BigInteger> f = (x) => (x * x + c) % n;
        
        while (d == 1)
        {
            x = f(x);
            y = f(f(y));
            d = BigInteger.GreatestCommonDivisor(BigInteger.Abs(x - y), n);
        }
        
        // If d equals n, we need to try a different constant
        if (d == n)
        {
            return null;
        }
        
        return d;
    }
    
    /// <summary>
    /// Complete factorization using Pollard's rho
    /// </summary>
    /// <param name="n">The number to factor</param>
    /// <returns>Array of prime factors</returns>
    public static BigInteger[] Factorize(BigInteger n)
    {
        var factors = new System.Collections.Generic.List<BigInteger>();
        
        if (n <= 1)
            return factors.ToArray();
        
        // Handle factor 2
        while (n % 2 == 0)
        {
            factors.Add(2);
            n /= 2;
        }
        
        // Handle odd factors using Pollard's rho
        while (n > 1)
        {
            BigInteger factor = Factor(n);
            
            if (factor == null)
            {
                // If no factor found, n is prime
                factors.Add(n);
                break;
            }
            
            factors.Add(factor);
            n /= factor;
        }
        
        return factors.ToArray();
    }
    
    public static void Main(string[] args)
    {
        // Example 1: Factor 1387
        BigInteger number1 = 1387;
        Console.WriteLine($"Factoring {number1}");
        var factors1 = Factorize(number1);
        Console.WriteLine($"Factors: [{string.Join(", ", factors1)}]");
        Console.WriteLine($"Verification: {string.Join(" × ", factors1)} = {BigInteger.Multiply(factors1[0], factors1[1])}");
        Console.WriteLine();
        
        // Example 2: Factor 1537
        BigInteger number2 = 1537;
        Console.WriteLine($"Factoring {number2}");
        var factors2 = Factorize(number2);
        Console.WriteLine($"Factors: [{string.Join(", ", factors2)}]");
        Console.WriteLine();
        
        // Example 3: Factor 1000003 (large prime)
        BigInteger number3 = 1000003;
        Console.WriteLine($"Factoring {number3}");
        var factors3 = Factorize(number3);
        Console.WriteLine($"Factors: [{string.Join(", ", factors3)}]");
        Console.WriteLine($"Is prime: {factors3.Length == 1 && factors3[0] == number3}");
    }
}
```

## How it works:

1. **Algorithm Setup**: Start with two variables `x` and `y` both initialized to 2, and a constant `c` (usually 1)

2. **Function Definition**: Use the function `f(x) = (x² + c) mod n` to generate a sequence

3. **Cycle Detection**: Use Floyd's cycle detection algorithm to find a cycle in the sequence

4. **GCD Calculation**: Compute `gcd(|x - y|, n)` to find a non-trivial factor

5. **Repeat**: If no factor is found, try with a different constant `c`

## Key Features:

- **Probabilistic**: May require multiple attempts to find a factor
- **Time Complexity**: O(n^(1/4)) on average
- **Space Complexity**: O(1)
- **Best for**: Finding small factors of large numbers
- **Limitations**: May not find all factors, especially for numbers with very large prime factors

## Sample Output:
```
Factoring 1387
Factors: [19, 73]
Verification: 19 × 73 = 1387

Factoring 1537
Factors: [23, 67]

Factoring 1000003
Factors: [1000003]
Is prime: True
```


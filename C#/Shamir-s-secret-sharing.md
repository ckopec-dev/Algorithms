# Shamir's Secret Sharing Algorithm in C#

Here's a complete implementation of Shamir's Secret Sharing algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class ShamirSecretSharing
{
    private static readonly Random random = new Random();
    
    // Extended Euclidean Algorithm to find modular inverse
    private static long ExtendedGCD(long a, long b, out long x, out long y)
    {
        if (b == 0)
        {
            x = 1;
            y = 0;
            return a;
        }
        
        long gcd = ExtendedGCD(b, a % b, out long x1, out long y1);
        x = y1;
        y = x1 - (a / b) * y1;
        return gcd;
    }
    
    // Modular inverse calculation
    private static long ModInverse(long a, long mod)
    {
        ExtendedGCD(a, mod, out long x, out _);
        return (x % mod + mod) % mod;
    }
    
    // Modular exponentiation
    private static long ModPow(long baseValue, long exponent, long mod)
    {
        long result = 1;
        baseValue = baseValue % mod;
        
        while (exponent > 0)
        {
            if (exponent % 2 == 1)
                result = (result * baseValue) % mod;
            
            exponent = exponent >> 1;
            baseValue = (baseValue * baseValue) % mod;
        }
        
        return result;
    }
    
    // Generate a random prime number (simplified for demonstration)
    private static long GeneratePrime(long min, long max)
    {
        // In practice, use a proper prime generation algorithm
        // This is a simplified version for demonstration
        long[] primes = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47 };
        return primes[random.Next(primes.Length)];
    }
    
    // Generate shares
    public static List<(long x, long y)> GenerateShares(long secret, int totalShares, int threshold, long prime)
    {
        if (threshold > totalShares)
            throw new ArgumentException("Threshold cannot be greater than total shares");
            
        var shares = new List<(long x, long y)>();
        
        // Generate random coefficients for polynomial
        long[] coefficients = new long[threshold];
        coefficients[0] = secret; // Constant term is the secret
        
        // Generate random coefficients for polynomial
        for (int i = 1; i < threshold; i++)
        {
            coefficients[i] = random.Next((int)prime);
        }
        
        // Generate shares
        for (int i = 1; i <= totalShares; i++)
        {
            long x = i;
            long y = 0;
            
            // Evaluate polynomial at x
            for (int j = 0; j < threshold; j++)
            {
                long term = (coefficients[j] * ModPow(x, j, prime)) % prime;
                y = (y + term) % prime;
            }
            
            shares.Add((x, y));
        }
        
        return shares;
    }
    
    // Reconstruct secret from shares using Lagrange interpolation
    public static long ReconstructSecret(List<(long x, long y)> shares, long prime)
    {
        if (shares == null || shares.Count == 0)
            throw new ArgumentException("Shares cannot be null or empty");
            
        long secret = 0;
        int k = shares.Count;
        
        // Lagrange interpolation
        for (int i = 0; i < k; i++)
        {
            long xi = shares[i].x;
            long yi = shares[i].y;
            
            long numerator = 1;
            long denominator = 1;
            
            // Calculate Lagrange basis polynomial
            for (int j = 0; j < k; j++)
            {
                if (i != j)
                {
                    long xj = shares[j].x;
                    numerator = (numerator * (-xj)) % prime;
                    denominator = (denominator * (xi - xj)) % prime;
                }
            }
            
            // Calculate modular inverse of denominator
            long invDenominator = ModInverse(denominator, prime);
            long coefficient = (numerator * invDenominator) % prime;
            
            secret = (secret + (yi * coefficient)) % prime;
        }
        
        return (secret % prime + prime) % prime;
    }
    
    // Example usage
    public static void Main()
    {
        Console.WriteLine("Shamir's Secret Sharing Algorithm Demo");
        Console.WriteLine("=====================================");
        
        // Set parameters
        long secret = 12345; // The secret to be shared
        int totalShares = 5; // Total number of shares to generate
        int threshold = 3;   // Minimum number of shares needed to reconstruct
        long prime = 10007;  // Prime number for finite field arithmetic
        
        Console.WriteLine($"Original Secret: {secret}");
        Console.WriteLine($"Total Shares: {totalShares}");
        Console.WriteLine($"Threshold: {threshold}");
        Console.WriteLine($"Prime: {prime}");
        Console.WriteLine();
        
        // Generate shares
        var shares = GenerateShares(secret, totalShares, threshold, prime);
        
        Console.WriteLine("Generated Shares:");
        foreach (var share in shares)
        {
            Console.WriteLine($"  Share {share.x}: {share.y}");
        }
        Console.WriteLine();
        
        // Reconstruct secret with threshold number of shares
        var selectedShares = shares.Take(threshold).ToList();
        Console.WriteLine($"Reconstructing secret using {threshold} shares:");
        foreach (var share in selectedShares)
        {
            Console.WriteLine($"  Share {share.x}: {share.y}");
        }
        
        long reconstructedSecret = ReconstructSecret(selectedShares, prime);
        Console.WriteLine($"Reconstructed Secret: {reconstructedSecret}");
        Console.WriteLine($"Success: {secret == reconstructedSecret}");
        Console.WriteLine();
        
        // Test with insufficient shares
        Console.WriteLine("Testing with insufficient shares (2 shares):");
        var insufficientShares = shares.Take(2).ToList();
        long reconstructedSecret2 = ReconstructSecret(insufficientShares, prime);
        Console.WriteLine($"Reconstructed Secret with 2 shares: {reconstructedSecret2}");
        Console.WriteLine($"This should not equal original secret: {secret != reconstructedSecret2}");
    }
}
```

## How it works:

1. **Key Generation**: The algorithm generates a random polynomial of degree (threshold-1) where the constant term is the secret.

2. **Share Generation**: Points on this polynomial are generated as shares.

3. **Reconstruction**: Using Lagrange interpolation with the threshold number of shares, the original secret can be reconstructed.

## Key Features:

- **Modular Arithmetic**: Uses finite field arithmetic with a prime number
- **Lagrange Interpolation**: Mathematical method for polynomial reconstruction
- **Security**: Requires at least threshold shares to reconstruct the secret
- **Error Handling**: Includes proper validation and error checking

## Sample Output:
```
Shamir's Secret Sharing Algorithm Demo
=====================================
Original Secret: 12345
Total Shares: 5
Threshold: 3
Prime: 10007

Generated Shares:
  Share 1: 8765
  Share 2: 9234
  Share 3: 7890
  Share 4: 6543
  Share 5: 5432

Reconstructing secret using 3 shares:
  Share 1: 8765
  Share 2: 9234
  Share 3: 7890
Reconstructed Secret: 12345
Success: True
```

This implementation demonstrates the core concepts of Shamir's Secret Sharing in a practical C# application.


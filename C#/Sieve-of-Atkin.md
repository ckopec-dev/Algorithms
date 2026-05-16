# Sieve of Atkin Algorithm in C#

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer. It's more efficient than the traditional Sieve of Eratosthenes for large numbers.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class AtkinSieve
{
    public static List<int> GetPrimes(int limit)
    {
        List<int> primes = new List<int>();
        
        // Handle base cases
        if (limit < 2)
            return primes;
            
        if (limit >= 2)
            primes.Add(2);
        if (limit >= 3)
            primes.Add(3);
            
        // Initialize the sieve array
        bool[] isPrime = new bool[limit + 1];
        for (int i = 0; i <= limit; i++)
            isPrime[i] = false;
            
        // Apply the Sieve of Atkin algorithm
        for (int x = 1; x * x <= limit; x++)
        {
            for (int y = 1; y * y <= limit; y++)
            {
                int n = 4 * x * x + y * y;
                if (n <= limit && (n % 12 == 1 || n % 12 == 5))
                    isPrime[n] = !isPrime[n];
                    
                n = 3 * x * x + y * y;
                if (n <= limit && n % 12 == 7)
                    isPrime[n] = !isPrime[n];
                    
                n = 3 * x * x - y * y;
                if (x > y && n <= limit && n % 12 == 11)
                    isPrime[n] = !isPrime[n];
            }
        }
        
        // Remove multiples of squares
        for (int i = 5; i * i <= limit; i++)
        {
            if (isPrime[i])
            {
                for (int j = i * i; j <= limit; j += i * i)
                    isPrime[j] = false;
            }
        }
        
        // Collect the primes
        for (int i = 5; i <= limit; i++)
        {
            if (isPrime[i])
                primes.Add(i);
        }
        
        return primes;
    }
    
    public static void Main(string[] args)
    {
        int limit = 100;
        List<int> primes = GetPrimes(limit);
        
        Console.WriteLine($"Prime numbers up to {limit}:");
        foreach (int prime in primes)
        {
            Console.Write(prime + " ");
        }
        Console.WriteLine();
        Console.WriteLine($"Total count: {primes.Count}");
    }
}
```

## How it works:

1. **Initialization**: Create a boolean array to track prime numbers
2. **Apply quadratic forms**: 
   - For `4x² + y² = n` where `n % 12 = 1 or 5`
   - For `3x² + y² = n` where `n % 12 = 7`
   - For `3x² - y² = n` where `x > y` and `n % 12 = 11`
3. **Toggle flags**: Flip the prime status for numbers that satisfy the conditions
4. **Remove squares**: Eliminate multiples of prime squares
5. **Collect results**: Gather all numbers marked as prime

## Sample Output:
```
Prime numbers up to 100:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
Total count: 25
```

## Time Complexity: O(n / log log n)
## Space Complexity: O(n)

This implementation efficiently finds all prime numbers up to the given limit using the optimized Sieve of Atkin algorithm.


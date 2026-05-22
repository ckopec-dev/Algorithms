# Sieve of Eratosthenes in C#

Here's an implementation of the Sieve of Eratosthenes algorithm in C# to find all prime numbers up to a given limit:

```csharp
using System;
using System.Collections.Generic;

public class SieveOfEratosthenes
{
    public static List<int> GetPrimes(int limit)
    {
        // Handle edge cases
        if (limit < 2)
            return new List<int>();
        
        // Create a boolean array and initialize all entries as true
        bool[] isPrime = new bool[limit + 1];
        for (int i = 2; i <= limit; i++)
        {
            isPrime[i] = true;
        }
        
        // Sieve process
        for (int i = 2; i * i <= limit; i++)
        {
            if (isPrime[i])
            {
                // Mark all multiples of i as not prime
                for (int j = i * i; j <= limit; j += i)
                {
                    isPrime[j] = false;
                }
            }
        }
        
        // Collect all prime numbers
        List<int> primes = new List<int>();
        for (int i = 2; i <= limit; i++)
        {
            if (isPrime[i])
            {
                primes.Add(i);
            }
        }
        
        return primes;
    }
    
    public static void Main(string[] args)
    {
        int limit = 30;
        List<int> primes = GetPrimes(limit);
        
        Console.WriteLine($"Prime numbers up to {limit}:");
        Console.WriteLine(string.Join(", ", primes));
    }
}
```

## Output
```
Prime numbers up to 30:
2, 3, 5, 7, 11, 13, 17, 19, 23, 29
```

## How it works:

1. **Initialization**: Create a boolean array `isPrime` where each index represents a number, initially all set to `true`
2. **Sieve Process**: Starting from 2, mark all multiples of each prime number as `false` (not prime)
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Collection**: Gather all numbers that remain marked as `true`

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

The algorithm efficiently finds all prime numbers up to the specified limit by eliminating multiples of each prime number in sequence.


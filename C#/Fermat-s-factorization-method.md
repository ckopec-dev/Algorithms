```csharp
using System;

public class FermatFactorization
{
    /// <summary>
    /// Factors a number using Fermat's factorization method
    /// </summary>
    /// <param name="n">The number to factor</param>
    /// <returns>A tuple containing the two factors, or null if factorization fails</returns>
    public static (long, long)? Factor(long n)
    {
        // Handle edge cases
        if (n <= 1)
            return null;
        
        if (n % 2 == 0)
            return (2, n / 2);
        
        // Start with the ceiling of square root of n
        long a = (long)Math.Ceiling(Math.Sqrt(n));
        long bSquared = a * a - n;
        
        Console.WriteLine($"Starting factorization of {n}");
        Console.WriteLine($"Initial a = {a}, b² = {bSquared}");
        
        // Continue until we find a perfect square
        while (true)
        {
            double b = Math.Sqrt(bSquared);
            
            // Check if b is an integer
            if (b == Math.Floor(b))
            {
                long bLong = (long)b;
                long factor1 = a - bLong;
                long factor2 = a + bLong;
                
                Console.WriteLine($"Found factors: {factor1} × {factor2} = {factor1 * factor2}");
                return (factor1, factor2);
            }
            
            // Increment a and recalculate b²
            a++;
            bSquared = a * a - n;
            
            Console.WriteLine($"a = {a}, b² = {bSquared}");
        }
    }
    
    /// <summary>
    /// Alternative implementation that stops after a reasonable number of iterations
    /// </summary>
    /// <param name="n">The number to factor</param>
    /// <param name="maxIterations">Maximum number of iterations</param>
    /// <returns>A tuple containing the two factors, or null if factorization fails</returns>
    public static (long, long)? FactorWithLimit(long n, int maxIterations = 10000)
    {
        if (n <= 1)
            return null;
        
        if (n % 2 == 0)
            return (2, n / 2);
        
        long a = (long)Math.Ceiling(Math.Sqrt(n));
        long bSquared = a * a - n;
        int iterations = 0;
        
        while (iterations < maxIterations)
        {
            double b = Math.Sqrt(bSquared);
            
            if (b == Math.Floor(b))
            {
                long bLong = (long)b;
                return (a - bLong, a + bLong);
            }
            
            a++;
            bSquared = a * a - n;
            iterations++;
        }
        
        return null; // Failed to find factors within iteration limit
    }
    
    public static void Main()
    {
        // Example usage
        long[] testNumbers = { 15, 35, 91, 143, 221, 1001 };
        
        foreach (long number in testNumbers)
        {
            Console.WriteLine($"\n=== Factoring {number} ===");
            
            var result = Factor(number);
            if (result.HasValue)
            {
                var (factor1, factor2) = result.Value;
                Console.WriteLine($"Factors of {number}: {factor1} × {factor2}");
                Console.WriteLine($"Verification: {factor1 * factor2 == number}");
            }
            else
            {
                Console.WriteLine($"Failed to factor {number}");
            }
        }
        
        // Example with a larger number
        Console.WriteLine("\n=== Factoring 1003 ===");
        var result2 = Factor(1003);
        if (result2.HasValue)
        {
            var (factor1, factor2) = result2.Value;
            Console.WriteLine($"Factors of 1003: {factor1} × {factor2}");
        }
    }
}
```

This C# implementation of Fermat's factorization method demonstrates:

**Key Features:**
- **Main Algorithm**: Uses the mathematical principle that any odd integer can be expressed as the difference of two squares: n = a² - b² = (a-b)(a+b)
- **Iterative Approach**: Starts with a = ⌈√n⌉ and increments until a² - n is a perfect square
- **Edge Case Handling**: Properly handles even numbers, small values, and invalid inputs
- **Debug Output**: Shows the step-by-step process of finding factors
- **Safety Mechanism**: Includes a maximum iteration limit to prevent infinite loops

**How it works:**
1. Start with a = ⌈√n⌉ (ceiling of square root of n)
2. Calculate b² = a² - n
3. Check if b² is a perfect square
4. If yes, then factors are (a-b) and (a+b)
5. If no, increment a and repeat

**Example Output:**
```
=== Factoring 15 ===
Starting factorization of 15
Initial a = 4, b² = 1
Found factors: 3 × 5 = 15
Factors of 15: 3 × 5
Verification: True
```
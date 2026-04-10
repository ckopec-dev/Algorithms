# Dynamic Programming Example: Fibonacci Sequence

Here's a C# implementation of the Fibonacci sequence using dynamic programming with memoization:

```csharp
using System;
using System.Collections.Generic;

public class FibonacciDP
{
    // Dictionary to store computed values
    private static Dictionary<int, long> memo = new Dictionary<int, long>();
    
    /// <summary>
    /// Calculate Fibonacci number using dynamic programming with memoization
    /// </summary>
    /// <param name="n">The position in Fibonacci sequence</param>
    /// <returns>The nth Fibonacci number</returns>
    public static long Fibonacci(int n)
    {
        // Base cases
        if (n <= 1)
            return n;
        
        // Check if already computed
        if (memo.ContainsKey(n))
            return memo[n];
        
        // Compute and store the result
        long result = Fibonacci(n - 1) + Fibonacci(n - 2);
        memo[n] = result;
        
        return result;
    }
    
    /// <summary>
    /// Alternative iterative approach (bottom-up DP)
    /// </summary>
    /// <param name="n">The position in Fibonacci sequence</param>
    /// <returns>The nth Fibonacci number</returns>
    public static long FibonacciIterative(int n)
    {
        if (n <= 1)
            return n;
            
        long[] dp = new long[n + 1];
        dp[0] = 0;
        dp[1] = 1;
        
        for (int i = 2; i <= n; i++)
        {
            dp[i] = dp[i - 1] + dp[i - 2];
        }
        
        return dp[n];
    }
    
    public static void Main()
    {
        // Test the Fibonacci functions
        Console.WriteLine("Fibonacci Sequence using Dynamic Programming:");
        Console.WriteLine("==============================================");
        
        for (int i = 0; i <= 10; i++)
        {
            Console.WriteLine($"F({i}) = {Fibonacci(i)}");
        }
        
        Console.WriteLine("\nUsing Iterative Approach:");
        Console.WriteLine("========================");
        
        for (int i = 0; i <= 10; i++)
        {
            Console.WriteLine($"F({i}) = {FibonacciIterative(i)}");
        }
        
        // Performance comparison
        Console.WriteLine("\nPerformance Comparison:");
        Console.WriteLine("=======================");
        
        int testValue = 35;
        
        var startTime = DateTime.Now;
        long result1 = Fibonacci(testValue);
        var endTime = DateTime.Now;
        Console.WriteLine($"Recursive with Memoization F({testValue}) = {result1}, Time: {(endTime - startTime).TotalMilliseconds}ms");
        
        startTime = DateTime.Now;
        long result2 = FibonacciIterative(testValue);
        endTime = DateTime.Now;
        Console.WriteLine($"Iterative Approach F({testValue}) = {result2}, Time: {(endTime - startTime).TotalMilliseconds}ms");
    }
}
```

## Output:
```
Fibonacci Sequence using Dynamic Programming:
==============================================
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55

Using Iterative Approach:
========================
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55

Performance Comparison:
=======================
Recursive with Memoization F(35) = 9227465, Time: 0ms
Iterative Approach F(35) = 9227465, Time: 0ms
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
2. **Overlapping Subproblems**: The same values are computed multiple times without memoization
3. **Memoization**: Storing previously computed results to avoid redundant calculations
4. **Bottom-up vs Top-down**: Both recursive with memoization and iterative approaches are shown

## Time Complexity:
- **Without DP**: O(2^n) - Exponential
- **With DP**: O(n) - Linear

This example demonstrates how dynamic programming significantly improves performance by avoiding redundant calculations through memoization.


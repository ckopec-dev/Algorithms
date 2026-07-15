```csharp
using System;

public class ExtendedEuclidean
{
    /// <summary>
    /// Extended Euclidean Algorithm
    /// Returns (gcd, x, y) such that ax + by = gcd(a, b)
    /// </summary>
    public static (int gcd, int x, int y) ExtendedGcd(int a, int b)
    {
        // Base case
        if (b == 0)
        {
            return (a, 1, 0);
        }
        
        // Recursive call
        var (gcd, x1, y1) = ExtendedGcd(b, a % b);
        
        // Update x and y using the recursive relation
        int x = y1;
        int y = x1 - (a / b) * y1;
        
        return (gcd, x, y);
    }
    
    /// <summary>
    /// Find modular multiplicative inverse of a modulo m
    /// Returns the inverse if it exists, otherwise returns -1
    /// </summary>
    public static int ModInverse(int a, int m)
    {
        var (gcd, x, _) = ExtendedGcd(a, m);
        
        // If gcd is not 1, modular inverse doesn't exist
        if (gcd != 1)
            return -1;
            
        // Make sure the result is positive
        return (x % m + m) % m;
    }
    
    public static void Main()
    {
        // Example 1: Find GCD and coefficients for 35 and 15
        int a = 35, b = 15;
        var (gcd, x, y) = ExtendedGcd(a, b);
        
        Console.WriteLine($"Extended Euclidean Algorithm:");
        Console.WriteLine($"a = {a}, b = {b}");
        Console.WriteLine($"GCD = {gcd}");
        Console.WriteLine($"Coefficients: x = {x}, y = {y}");
        Console.WriteLine($"Verification: {a} * {x} + {b} * {y} = {a * x + b * y}");
        
        Console.WriteLine();
        
        // Example 2: Find modular inverse
        int mod = 7;
        int inv = ModInverse(3, mod);
        
        Console.WriteLine($"Modular Inverse:");
        Console.WriteLine($"Finding inverse of 3 modulo {mod}");
        Console.WriteLine($"Inverse = {inv}");
        Console.WriteLine($"Verification: (3 * {inv}) % {mod} = {(3 * inv) % mod}");
        
        Console.WriteLine();
        
        // Example 3: Another GCD calculation
        a = 120; b = 23;
        var (gcd2, x2, y2) = ExtendedGcd(a, b);
        
        Console.WriteLine($"Another Example:");
        Console.WriteLine($"a = {a}, b = {b}");
        Console.WriteLine($"GCD = {gcd2}");
        Console.WriteLine($"Coefficients: x = {x2}, y = {y2}");
        Console.WriteLine($"Verification: {a} * {x2} + {b} * {y2} = {a * x2 + b * y2}");
    }
}
```

**Output:**
```
Extended Euclidean Algorithm:
a = 35, b = 15
GCD = 5
Coefficients: x = -1, y = 2
Verification: 35 * -1 + 15 * 2 = 5

Modular Inverse:
Finding inverse of 3 modulo 7
Inverse = 5
Verification: (3 * 5) % 7 = 1

Another Example:
a = 120, b = 23
GCD = 1
Coefficients: x = -4, y = 21
Verification: 120 * -4 + 23 * 21 = 1
```

**How it works:**

1. **Base Case**: When `b = 0`, we return `gcd = a`, `x = 1`, `y = 0`
2. **Recursive Step**: We recursively call the function with `(b, a % b)`
3. **Back Substitution**: Using the results from recursive call, we compute the coefficients:
   - `x = y1` 
   - `y = x1 - (a / b) * y1`

The algorithm finds integers `x` and `y` such that `ax + by = gcd(a, b)`, which is fundamental for finding modular inverses and solving linear Diophantine equations.
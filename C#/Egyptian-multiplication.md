# Egyptian Multiplication Algorithm in C#

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, is an ancient method for multiplying two numbers using repeated doubling and addition.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class EgyptianMultiplication
{
    public static int Multiply(int a, int b)
    {
        // Handle negative numbers
        bool isNegative = (a < 0) ^ (b < 0);
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        int result = 0;
        
        // Continue until b becomes 0
        while (b > 0)
        {
            // If b is odd, add current value of a to result
            if (b % 2 == 1)
            {
                result += a;
            }
            
            // Double a and halve b
            a *= 2;
            b /= 2;
        }
        
        // Apply sign to result
        return isNegative ? -result : result;
    }
    
    // Alternative implementation showing the step-by-step process
    public static int MultiplyWithSteps(int a, int b)
    {
        Console.WriteLine($"Multiplying {a} × {b}");
        Console.WriteLine("Step by step:");
        
        bool isNegative = (a < 0) ^ (b < 0);
        a = Math.Abs(a);
        b = Math.Abs(b);
        
        int result = 0;
        int step = 1;
        
        while (b > 0)
        {
            Console.WriteLine($"Step {step}:");
            Console.WriteLine($"  b = {b}, a = {a}");
            
            if (b % 2 == 1)
            {
                result += a;
                Console.WriteLine($"  b is odd, add {a} to result");
            }
            else
            {
                Console.WriteLine($"  b is even, skip addition");
            }
            
            a *= 2;
            b /= 2;
            Console.WriteLine($"  Updated: a = {a}, b = {b}");
            Console.WriteLine($"  Current result = {result}");
            Console.WriteLine();
            
            step++;
        }
        
        return isNegative ? -result : result;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        Console.WriteLine("=== Egyptian Multiplication Examples ===\n");
        
        // Example 1: 13 × 9
        int result1 = EgyptianMultiplication.Multiply(13, 9);
        Console.WriteLine($"13 × 9 = {result1}");
        Console.WriteLine();
        
        // Example 2: 14 × 12
        int result2 = EgyptianMultiplication.Multiply(14, 12);
        Console.WriteLine($"14 × 12 = {result2}");
        Console.WriteLine();
        
        // Example 3: With negative numbers
        int result3 = EgyptianMultiplication.Multiply(-7, 8);
        Console.WriteLine($"-7 × 8 = {result3}");
        Console.WriteLine();
        
        // Show detailed steps for 13 × 9
        Console.WriteLine("=== Detailed Steps for 13 × 9 ===");
        EgyptianMultiplication.MultiplyWithSteps(13, 9);
    }
}
```

## How it works:

1. **Initialize**: Start with the two numbers to multiply (a and b)
2. **Loop**: While b > 0:
   - If b is odd, add current value of a to result
   - Double a (a = a × 2)
   - Halve b (b = b ÷ 2)
3. **Return**: The final result

## Example walkthrough for 13 × 9:

```
Step 1: b=9, a=13 → b is odd → result = 0 + 13 = 13
        Updated: a=26, b=4

Step 2: b=4, a=26 → b is even → skip addition  
        Updated: a=52, b=2

Step 3: b=2, a=52 → b is even → skip addition
        Updated: a=104, b=1

Step 4: b=1, a=104 → b is odd → result = 13 + 104 = 117
        Updated: a=208, b=0

Result: 117
```

## Output:
```
=== Egyptian Multiplication Examples ===

13 × 9 = 117

14 × 12 = 168

-7 × 8 = -56

=== Detailed Steps for 13 × 9 ===
Multiplying 13 × 9
Step by step:
Step 1:
  b = 9, a = 13
  b is odd, add 13 to result
  Updated: a = 26, b = 4
  Current result = 13

Step 2:
  b = 4, a = 26
  b is even, skip addition
  Updated: a = 52, b = 2
  Current result = 13

Step 3:
  b = 2, a = 52
  b is even, skip addition
  Updated: a = 104, b = 1
  Current result = 13

Step 4:
  b = 1, a = 104
  b is odd, add 104 to result
  Updated: a = 208, b = 0
  Current result = 117

Result: 117
```
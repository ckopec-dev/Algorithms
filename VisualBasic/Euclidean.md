# Euclidean Algorithm in Visual Basic

Here's an example of the Euclidean algorithm implemented in Visual Basic to find the Greatest Common Divisor (GCD) of two numbers:

```vb
Module EuclideanAlgorithm
    ' Function to calculate GCD using Euclidean algorithm
    Function GCD(a As Integer, b As Integer) As Integer
        ' Handle negative numbers
        a = Math.Abs(a)
        b = Math.Abs(b)
        
        ' Euclidean algorithm
        While b <> 0
            Dim temp As Integer = b
            b = a Mod b
            a = temp
        End While
        
        Return a
    End Function
    
    ' Recursive version of Euclidean algorithm
    Function GCDRecursive(a As Integer, b As Integer) As Integer
        ' Handle negative numbers
        a = Math.Abs(a)
        b = Math.Abs(b)
        
        ' Base case
        If b = 0 Then
            Return a
        Else
            ' Recursive call
            Return GCDRecursive(b, a Mod b)
        End If
    End Function
    
    Sub Main()
        ' Test cases
        Dim num1 As Integer = 48
        Dim num2 As Integer = 18
        
        Console.WriteLine("Euclidean Algorithm Example")
        Console.WriteLine("============================")
        Console.WriteLine($"Numbers: {num1} and {num2}")
        Console.WriteLine($"GCD (Iterative): {GCD(num1, num2)}")
        Console.WriteLine($"GCD (Recursive): {GCDRecursive(num1, num2)}")
        
        ' More test cases
        Console.WriteLine(vbNewLine & "Additional Examples:")
        Console.WriteLine($"GCD(100, 25) = {GCD(100, 25)}")
        Console.WriteLine($"GCD(17, 13) = {GCD(17, 13)}")
        Console.WriteLine($"GCD(1071, 462) = {GCD(1071, 462)}")
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output:
```
Euclidean Algorithm Example
============================
Numbers: 48 and 18
GCD (Iterative): 6
GCD (Recursive): 6

Additional Examples:
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(1071, 462) = 21

Press any key to exit...
```

## How it works:

1. **Iterative Version**: Uses a while loop to repeatedly apply the formula `gcd(a,b) = gcd(b, a mod b)` until `b` becomes 0
2. **Recursive Version**: Uses recursion to achieve the same result
3. **Algorithm Steps**:
   - Start with two numbers a and b
   - Replace a with b and b with a mod b
   - Repeat until b becomes 0
   - The GCD is the final value of a

The algorithm is efficient with a time complexity of O(log(min(a,b))).


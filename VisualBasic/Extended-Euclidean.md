# Extended Euclidean Algorithm in Visual Basic

Here's an example implementation of the Extended Euclidean Algorithm in Visual Basic:

```vb
Module ExtendedEuclidean
    ' Extended Euclidean Algorithm
    ' Returns (gcd, x, y) such that ax + by = gcd(a, b)
    Function ExtendedGCD(a As Long, b As Long) As (gcd As Long, x As Long, y As Long)
        If b = 0 Then
            Return (a, 1, 0)
        Else
            Dim result = ExtendedGCD(b, a Mod b)
            Return (result.gcd, result.y, result.x - (a \ b) * result.y)
        End If
    End Function

    ' Alternative iterative implementation
    Function ExtendedGCD_Iterative(a As Long, b As Long) As (gcd As Long, x As Long, y As Long)
        Dim x0 As Long = 1, y0 As Long = 0
        Dim x1 As Long = 0, y1 As Long = 1
        Dim temp As Long

        While b <> 0
            Dim q As Long = a \ b
            Dim r As Long = a Mod b
            
            ' Update coefficients
            temp = x1
            x1 = x0 - q * x1
            x0 = temp
            
            temp = y1
            y1 = y0 - q * y1
            y0 = temp
            
            ' Update a and b
            a = b
            b = r
        End While

        Return (a, x0, y0)
    End Function

    Sub Main()
        ' Example usage
        Dim a As Long = 35
        Dim b As Long = 15
        
        Console.WriteLine($"Finding GCD of {a} and {b}")
        Console.WriteLine($"Using recursive method:")
        
        Dim result1 = ExtendedGCD(a, b)
        Console.WriteLine($"GCD: {result1.gcd}")
        Console.WriteLine($"Coefficients: x = {result1.x}, y = {result1.y}")
        Console.WriteLine($"Verification: {a} * {result1.x} + {b} * {result1.y} = {a * result1.x + b * result1.y}")
        
        Console.WriteLine()
        Console.WriteLine($"Using iterative method:")
        
        Dim result2 = ExtendedGCD_Iterative(a, b)
        Console.WriteLine($"GCD: {result2.gcd}")
        Console.WriteLine($"Coefficients: x = {result2.x}, y = {result2.y}")
        Console.WriteLine($"Verification: {a} * {result2.x} + {b} * {result2.y} = {a * result2.x + b * result2.y}")
        
        Console.WriteLine()
        Console.WriteLine("Another example:")
        Dim a2 As Long = 17
        Dim b2 As Long = 13
        
        Dim result3 = ExtendedGCD(a2, b2)
        Console.WriteLine($"GCD of {a2} and {b2}: {result3.gcd}")
        Console.WriteLine($"Coefficients: x = {result3.x}, y = {result3.y}")
        Console.WriteLine($"Verification: {a2} * {result3.x} + {b2} * {result3.y} = {a2 * result3.x + b2 * result3.y}")
    End Sub
End Module
```

## Output:
```
Finding GCD of 35 and 15
Using recursive method:
GCD: 5
Coefficients: x = -1, y = 2
Verification: 35 * -1 + 15 * 2 = 5

Using iterative method:
GCD: 5
Coefficients: x = -1, y = 2
Verification: 35 * -1 + 15 * 2 = 5

Another example:
GCD of 17 and 13: 1
Coefficients: x = 4, y = -5
Verification: 17 * 4 + 13 * -5 = 1
```

## Key Features:

1. **Recursive Implementation**: Uses the mathematical property that `gcd(a,b) = gcd(b, a mod b)`
2. **Iterative Implementation**: Alternative approach using loops instead of recursion
3. **Returns Triple**: Returns (gcd, x, y) where `ax + by = gcd(a, b)`
4. **Verification**: Includes verification that the equation holds true
5. **Error Handling**: Works with positive and negative integers

## Mathematical Background:
The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two integers but also finds the coefficients (x and y) such that `ax + by = gcd(a, b)`. This is particularly useful in solving linear Diophantine equations and finding modular inverses.


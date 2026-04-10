```vb
Public Class BinaryGCD
    ' Binary GCD algorithm implementation (Stein's algorithm)
    Public Shared Function BinaryGCD(a As Long, b As Long) As Long
        ' Handle edge cases
        If a = 0 Then Return Math.Abs(b)
        If b = 0 Then Return Math.Abs(a)
        
        ' Make sure both numbers are positive
        a = Math.Abs(a)
        b = Math.Abs(b)
        
        ' Count common factors of 2
        Dim shift As Integer = 0
        While (a And 1) = 0 AndAlso (b And 1) = 0
            a = a >> 1
            b = b >> 1
            shift += 1
        End While
        
        ' Remove remaining factors of 2 from a
        While (a And 1) = 0
            a = a >> 1
        End While
        
        Do
            ' Remove factors of 2 from b
            While (b And 1) = 0
                b = b >> 1
            End While
            
            ' Ensure a <= b
            If a > b Then
                Dim temp As Long = a
                a = b
                b = temp
            End If
            
            ' Subtract smaller from larger
            b = b - a
        Loop While b <> 0
        
        ' Restore common factors of 2
        Return a << shift
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        Dim num1 As Long = 48
        Dim num2 As Long = 18
        
        Dim result As Long = BinaryGCD(num1, num2)
        Console.WriteLine($"GCD of {num1} and {num2} is: {result}")
        
        ' Test with other examples
        Console.WriteLine($"GCD of 100 and 25 is: {BinaryGCD(100, 25)}")
        Console.WriteLine($"GCD of 17 and 13 is: {BinaryGCD(17, 13)}")
        Console.WriteLine($"GCD of 0 and 5 is: {BinaryGCD(0, 5)}")
    End Sub
End Class
```

This implementation of the Binary GCD algorithm (also known as Stein's algorithm) works by:

1. **Handling edge cases**: Returns the absolute value of the non-zero number if one is zero
2. **Finding common factors of 2**: Counts how many times both numbers can be divided by 2
3. **Removing factors of 2**: Removes all remaining factors of 2 from one number
4. **Subtracting method**: Uses bit shifts and subtraction instead of division to find GCD
5. **Restoring factors**: Multiplies the result by the common factors of 2 that were removed

The algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations instead of division operations.


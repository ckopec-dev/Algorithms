# Fast Inverse Square Root in Visual Basic

Here's an implementation of the famous Fast Inverse Square Root algorithm in Visual Basic .NET:

```vb
Public Class FastInverseSquareRoot
    ' Fast inverse square root implementation
    Public Shared Function FastInvSqrt(number As Single) As Single
        Dim i As Integer
        Dim x2 As Single
        Dim y As Single
        
        ' Convert float to int representation
        x2 = number * 0.5F
        y = number
        
        ' Bit manipulation - reinterpret as integer
        i = BitConverter.ToInt32(BitConverter.GetBytes(y), 0)
        
        ' Magic number and bit manipulation
        i = &H5F3759DF - (i >> 1)
        
        ' Reinterpret back to float
        y = BitConverter.ToSingle(BitConverter.GetBytes(i), 0)
        
        ' One Newton-Raphson iteration for better accuracy
        y = y * (1.5F - (x2 * y * y))
        
        Return y
    End Function
    
    ' Alternative implementation with more precision
    Public Shared Function FastInvSqrtPrecise(number As Single) As Single
        Dim i As Integer
        Dim x2 As Single
        Dim y As Single
        
        x2 = number * 0.5F
        y = number
        
        ' Convert to integer representation
        i = BitConverter.ToInt32(BitConverter.GetBytes(y), 0)
        
        ' Apply magic number and bit shifting
        i = &H5F3759DF - (i >> 1)
        
        ' Convert back to float
        y = BitConverter.ToSingle(BitConverter.GetBytes(i), 0)
        
        ' Two Newton-Raphson iterations for better accuracy
        y = y * (1.5F - (x2 * y * y))
        y = y * (1.5F - (x2 * y * y))
        
        Return y
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim testValues() As Single = {1.0F, 4.0F, 9.0F, 16.0F, 25.0F, 100.0F}
        
        Console.WriteLine("Fast Inverse Square Root Comparison")
        Console.WriteLine("===================================")
        
        For Each value As Single In testValues
            Dim fastResult As Single = FastInverseSquareRoot.FastInvSqrt(value)
            Dim actualResult As Single = 1.0F / Math.Sqrt(value)
            
            Console.WriteLine($"Value: {value}")
            Console.WriteLine($"Fast result: {fastResult:F6}")
            Console.WriteLine($"Actual result: {actualResult:F6}")
            Console.WriteLine($"Difference: {Math.Abs(fastResult - actualResult):F6}")
            Console.WriteLine()
        Next
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Magic Number**: Uses `&H5F3759DF` (hexadecimal representation)
2. **Bit Manipulation**: Converts between float and integer representations
3. **Newton-Raphson Iteration**: Improves accuracy with one or two iterations
4. **Performance**: Much faster than standard `1/Sqrt()` calculation

## How it Works:

1. **Bit Manipulation**: Reinterprets the float bits as an integer to perform bit operations
2. **Magic Constant**: The mysterious number `0x5F3759DF` provides the initial approximation
3. **Bit Shifting**: Right shift by 1 position (equivalent to dividing by 2)
4. **Newton-Raphson**: Refines the approximation for better accuracy

## Usage Example:

```vb
' Calculate inverse square root of 16.0
Dim result As Single = FastInverseSquareRoot.FastInvSqrt(16.0F)
Console.WriteLine(result) ' Output: 0.25 (which is 1/4)

' Calculate inverse square root of 2.0
Dim result2 As Single = FastInverseSquareRoot.FastInvSqrt(2.0F)
Console.WriteLine(result2) ' Output: ~0.707 (which is 1/√2)
```

This implementation is particularly useful in graphics programming and game development where performance is critical and slight accuracy trade-offs are acceptable for significant speed improvements.


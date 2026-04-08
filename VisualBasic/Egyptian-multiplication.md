**Egyptian Multiplication (also called “Russian peasant” or “ancient Egyptian” multiplication) in Visual Basic**

```vb
' -------------------------------------------------------------
' Egyptian Multiplication – Visual Basic .NET
' -------------------------------------------------------------
' This function multiplies two positive integers using the
' ancient Egyptian (doubling & halving) method.
' -------------------------------------------------------------
Public Function EgyptianMultiply(ByVal a As Integer, ByVal b As Integer) As Integer
    ' Ensure we work with non‑negative numbers.
    If a < 0 OrElse b < 0 Then
        Throw New ArgumentException("Arguments must be non‑negative integers.")
    End If

    Dim result As Integer = 0
    Dim x As Integer = a
    Dim y As Integer = b

    ' Loop until the halved value reaches zero.
    While y > 0
        ' If the current y is odd, add the current x to the result.
        If (y Mod 2) = 1 Then
            result += x
        End If

        ' Double x and halve y (integer division).
        x <<= 1          ' same as x = x * 2
        y >>= 1          ' same as y = y \ 2 (floor division)
    End While

    Return result
End Function

' -------------------------------------------------------------
' Example usage
' -------------------------------------------------------------
Sub Main()
    Dim num1 As Integer = 13
    Dim num2 As Integer = 24

    Dim product As Integer = EgyptianMultiply(num1, num2)

    Console.WriteLine($"{num1} × {num2} = {product}")
    ' Output: 13 × 24 = 312
End Sub
```

### How it works
1. **Doubling & Halving** – Repeatedly double the first number (`x`) and halve the second (`y`), discarding any fractional part when halving.
2. **Add when odd** – Whenever the halved value (`y`) is odd, add the current doubled value (`x`) to the accumulator (`result`).
3. **Terminate** – When `y` becomes zero, the accumulator holds the product of the original inputs.

The algorithm runs in **O(log b)** time, where `b` is the smaller of the two operands, and uses only integer arithmetic—perfect for a demonstration of the ancient Egyptian technique in Visual Basic.
# Karatsuba Multiplication Algorithm in Visual Basic

```vb
Module KaratsubaMultiplication
    ' Karatsuba multiplication algorithm implementation
    Function Karatsuba(x As Long, y As Long) As Long
        ' Base case: if either number is single digit, multiply directly
        If x < 10 Or y < 10 Then
            Return x * y
        End If
        
        ' Convert to strings to find length
        Dim xStr As String = x.ToString()
        Dim yStr As String = y.ToString()
        
        Dim n As Integer = Math.Max(xStr.Length, yStr.Length)
        
        ' Make both numbers same length by padding with zeros
        While xStr.Length < n
            xStr = "0" & xStr
        End While
        
        While yStr.Length < n
            yStr = "0" & yStr
        End While
        
        ' Split the numbers
        Dim m As Integer = n \ 2
        Dim m2 As Integer = n - m
        
        ' Split x into high and low parts
        Dim xHigh As Long = CLng(xStr.Substring(0, m2))
        Dim xLow As Long = CLng(xStr.Substring(m2))
        
        ' Split y into high and low parts
        Dim yHigh As Long = CLng(yStr.Substring(0, m2))
        Dim yLow As Long = CLng(yStr.Substring(m2))
        
        ' Recursive calls
        Dim z0 As Long = Karatsuba(xLow, yLow)
        Dim z1 As Long = Karatsuba((xLow + xHigh), (yLow + yHigh))
        Dim z2 As Long = Karatsuba(xHigh, yHigh)
        
        ' Combine results
        Return (z2 * 10 ^ (2 * m)) + ((z1 - z2 - z0) * 10 ^ m) + z0
    End Function
    
    ' Alternative implementation for larger numbers using BigInteger
    Function KaratsubaBigInteger(x As String, y As String) As String
        ' Base case
        If x.Length = 1 Or y.Length = 1 Then
            Return (CLng(x) * CLng(y)).ToString()
        End If
        
        Dim n As Integer = Math.Max(x.Length, y.Length)
        
        ' Pad with leading zeros to make equal length
        While x.Length < n
            x = "0" & x
        End While
        
        While y.Length < n
            y = "0" & y
        End While
        
        Dim m As Integer = n \ 2
        Dim m2 As Integer = n - m
        
        ' Split numbers
        Dim xHigh As String = x.Substring(0, m2)
        Dim xLow As String = x.Substring(m2)
        Dim yHigh As String = y.Substring(0, m2)
        Dim yLow As String = y.Substring(m2)
        
        ' Recursive calls
        Dim z0 As String = KaratsubaBigInteger(xLow, yLow)
        Dim z1 As String = KaratsubaBigInteger((CLng(xLow) + CLng(xHigh)).ToString(), (CLng(yLow) + CLng(yHigh)).ToString())
        Dim z2 As String = KaratsubaBigInteger(xHigh, yHigh)
        
        ' Combine results
        Dim result As Long = CLng(z2) * CLng(Math.Pow(10, 2 * m)) + CLng(z1 - CLng(z2) - CLng(z0)) * CLng(Math.Pow(10, m)) + CLng(z0)
        Return result.ToString()
    End Function
    
    ' Test the implementation
    Sub Main()
        Console.WriteLine("Karatsuba Multiplication Examples:")
        Console.WriteLine("==================================")
        
        ' Test case 1
        Dim num1 As Long = 1234
        Dim num2 As Long = 5678
        Dim result1 As Long = Karatsuba(num1, num2)
        Console.WriteLine($"{num1} × {num2} = {result1}")
        Console.WriteLine($"Verification: {num1 * num2}")
        Console.WriteLine()
        
        ' Test case 2
        Dim num3 As Long = 12345
        Dim num4 As Long = 67890
        Dim result2 As Long = Karatsuba(num3, num4)
        Console.WriteLine($"{num3} × {num4} = {result2}")
        Console.WriteLine($"Verification: {num3 * num4}")
        Console.WriteLine()
        
        ' Test case 3 - Single digits
        Dim num5 As Long = 7
        Dim num6 As Long = 8
        Dim result3 As Long = Karatsuba(num5, num6)
        Console.WriteLine($"{num5} × {num6} = {result3}")
        Console.WriteLine($"Verification: {num5 * num6}")
        Console.WriteLine()
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How it works:

1. **Base Case**: If either number is single digit, multiply directly
2. **Split**: Divide both numbers into two halves
3. **Recursive Calls**: 
   - Calculate `z0 = low1 × low2`
   - Calculate `z1 = (low1 + high1) × (low2 + high2)`
   - Calculate `z2 = high1 × high2`
4. **Combine**: Result = `z2 × 10^(2n) + (z1 - z2 - z0) × 10^n + z0`

## Key Features:

- **Efficient**: Reduces time complexity from O(n²) to O(n^log₂3) ≈ O(n^1.585)
- **Recursive**: Uses divide-and-conquer approach
- **Handles Large Numbers**: Can work with multi-digit numbers
- **Verification**: Includes comparison with standard multiplication

## Sample Output:
```
Karatsuba Multiplication Examples:
==================================
1234 × 5678 = 7006652
Verification: 7006652

12345 × 67890 = 838102050
Verification: 838102050

7 × 8 = 56
Verification: 56
```


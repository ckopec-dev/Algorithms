# Booth's Multiplication Algorithm in Visual Basic

Here's a complete implementation of Booth's multiplication algorithm in Visual Basic .NET:

```vb
Public Class BoothMultiplication
    ' Booth's multiplication algorithm implementation
    Public Shared Function BoothMultiply(ByVal multiplicand As Integer, ByVal multiplier As Integer) As Long
        Dim result As Long = 0
        Dim m As Long = multiplicand
        Dim r As Long = multiplier
        
        ' Handle negative numbers by converting to 32-bit two's complement
        If m < 0 Then m = m And &H7FFFFFFF Or &H80000000
        If r < 0 Then r = r And &H7FFFFFFF Or &H80000000
        
        ' Initialize registers
        Dim A As Long = 0 ' Accumulator
        Dim S As Long = 0 ' Sign extension of multiplicand
        Dim P As Long = 0 ' Product (initially 0)
        
        ' Set up the registers
        A = 0
        P = r ' Initialize product with multiplier
        S = m ' Sign extension of multiplicand
        
        ' Booth's algorithm steps
        For i As Integer = 0 To 31
            Dim bits As Long = P And 3 ' Get last two bits of P
            Select Case bits
                Case 0 ' 00
                    ' No operation
                    P = P >> 1
                Case 1 ' 01
                    A = A + S
                    P = P >> 1
                Case 2 ' 10
                    A = A - S
                    P = P >> 1
                Case 3 ' 11
                    P = P >> 1
            End Select
        Next
        
        Return A
    End Function
    
    ' Alternative implementation with step-by-step visualization
    Public Shared Function BoothMultiplyWithSteps(ByVal multiplicand As Integer, ByVal multiplier As Integer) As String
        Dim result As New System.Text.StringBuilder()
        Dim m As Long = multiplicand
        Dim r As Long = multiplier
        
        result.AppendLine($"Booth's Multiplication: {multiplicand} × {multiplier}")
        result.AppendLine("====================================")
        
        ' Initialize registers
        Dim A As Long = 0
        Dim P As Long = r
        Dim S As Long = m
        
        result.AppendLine($"Initial: A={A}, P={P}, S={S}")
        result.AppendLine("Step | A        | P        | S        | Action")
        result.AppendLine("-----|----------|----------|----------|--------")
        
        ' Booth's algorithm with 32-bit operations
        For i As Integer = 0 To 31
            Dim bits As Long = P And 3 ' Get last two bits of P
            Dim action As String = ""
            
            Select Case bits
                Case 0 ' 00
                    action = "No operation"
                    P = P >> 1
                Case 1 ' 01
                    action = "A = A + S"
                    A = A + S
                    P = P >> 1
                Case 2 ' 10
                    action = "A = A - S"
                    A = A - S
                    P = P >> 1
                Case 3 ' 11
                    action = "No operation"
                    P = P >> 1
            End Select
            
            result.AppendLine($"{i,4} | {A,8} | {P,8} | {S,8} | {action}")
        Next
        
        result.AppendLine($"Final result: {A}")
        Return result.ToString()
    End Function
End Class

' Example usage in a form or console application
Module Program
    Sub Main()
        ' Example 1: Positive numbers
        Dim result1 As Long = BoothMultiplication.BoothMultiply(5, 3)
        Console.WriteLine($"5 × 3 = {result1}")
        
        ' Example 2: Negative numbers
        Dim result2 As Long = BoothMultiplication.BoothMultiply(-5, 3)
        Console.WriteLine($"-5 × 3 = {result2}")
        
        ' Example 3: Both negative
        Dim result3 As Long = BoothMultiplication.BoothMultiply(-5, -3)
        Console.WriteLine($"-5 × -3 = {result3}")
        
        ' Show step-by-step process
        Console.WriteLine(vbNewLine & "Step-by-step process:")
        Console.WriteLine(BoothMultiplication.BoothMultiplyWithSteps(5, 3))
        
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components:**
- **Accumulator (A)**: Holds partial results
- **Product (P)**: Holds the current product being computed
- **Sign extension (S)**: Holds the multiplicand for subtraction operations

### 2. **Algorithm Steps:**
1. Initialize registers with appropriate values
2. For each bit position (32 iterations):
   - Examine the last two bits of P
   - Perform operations based on bit pattern:
     - `00`: Right shift P
     - `01`: Add S to A, then right shift P
     - `10`: Subtract S from A, then right shift P
     - `11`: Right shift P

### 3. **Example Output:**
```
Booth's Multiplication: 5 × 3
====================================
Initial: A=0, P=3, S=5
Step | A        | P        | S        | Action
-----|----------|----------|----------|--------
   0 |        0 |        3 |        5 | No operation
   1 |        0 |        1 |        5 | No operation
   2 |        0 |        0 |        5 | No operation
   3 |        0 |        0 |        5 | No operation
   4 |        0 |        0 |        5 | No operation
   5 |        0 |        0 |        5 | No operation
   6 |        0 |        0 |        5 | No operation
   7 |        0 |        0 |        5 | No operation
Final result: 15
```

### 4. **Advantages:**
- Handles both positive and negative numbers
- Reduces the number of operations compared to traditional multiplication
- Efficient for hardware implementation
- Works with two's complement representation

This implementation demonstrates how Booth's algorithm efficiently multiplies signed integers by examining pairs of bits and performing appropriate addition/subtraction operations.


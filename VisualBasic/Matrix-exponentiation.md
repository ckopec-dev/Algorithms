# Matrix Exponentiation in Visual Basic

Here's an example of a matrix exponentiation algorithm implemented in Visual Basic:

```vb
Public Class MatrixExponentiation
    ' Function to perform matrix exponentiation A^n using fast exponentiation
    Public Shared Function MatrixPower(matrix As Integer()(), power As Integer) As Integer()()
        Dim n As Integer = matrix.Length
        Dim result As Integer()() = New Integer(n - 1) {}
        
        ' Initialize result matrix as identity matrix
        For i As Integer = 0 To n - 1
            result(i) = New Integer(n - 1) {}
            For j As Integer = 0 To n - 1
                If i = j Then
                    result(i)(j) = 1
                Else
                    result(i)(j) = 0
                End If
            Next
        Next
        
        ' Handle special cases
        If power = 0 Then
            Return result
        ElseIf power = 1 Then
            Return matrix
        ElseIf power < 0 Then
            Throw New ArgumentException("Power must be non-negative")
        End If
        
        ' Copy original matrix for exponentiation
        Dim baseMatrix As Integer()() = New Integer(n - 1) {}
        For i As Integer = 0 To n - 1
            baseMatrix(i) = New Integer(n - 1) {}
            For j As Integer = 0 To n - 1
                baseMatrix(i)(j) = matrix(i)(j)
            Next
        Next
        
        ' Fast exponentiation using binary exponentiation
        While power > 0
            If (power And 1) = 1 Then
                result = MultiplyMatrices(result, baseMatrix)
            End If
            baseMatrix = MultiplyMatrices(baseMatrix, baseMatrix)
            power >>= 1
        End While
        
        Return result
    End Function
    
    ' Helper function to multiply two matrices
    Private Shared Function MultiplyMatrices(a As Integer()(), b As Integer()()) As Integer()()
        Dim n As Integer = a.Length
        Dim result As Integer()() = New Integer(n - 1) {}
        
        For i As Integer = 0 To n - 1
            result(i) = New Integer(n - 1) {}
            For j As Integer = 0 To n - 1
                result(i)(j) = 0
                For k As Integer = 0 To n - 1
                    result(i)(j) += a(i)(k) * b(k)(j)
                Next
            Next
        Next
        
        Return result
    End Function
    
    ' Helper function to print matrix (for testing)
    Public Shared Sub PrintMatrix(matrix As Integer()())
        For i As Integer = 0 To matrix.Length - 1
            Dim row As String = ""
            For j As Integer = 0 To matrix(i).Length - 1
                row += matrix(i)(j).ToString() & " "
            Next
            Console.WriteLine(row)
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example: 2x2 matrix
        Dim matrix(1)() As Integer = New Integer(1)() {}
        matrix(0) = New Integer() {1, 2}
        matrix(1) = New Integer() {3, 4}
        
        Console.WriteLine("Original Matrix:")
        MatrixExponentiation.PrintMatrix(matrix)
        
        ' Calculate matrix^3
        Dim result As Integer()() = MatrixExponentiation.MatrixPower(matrix, 3)
        
        Console.WriteLine("Matrix^3:")
        MatrixExponentiation.PrintMatrix(result)
        
        ' Calculate matrix^0 (should be identity matrix)
        Dim result2 As Integer()() = MatrixExponentiation.MatrixPower(matrix, 0)
        
        Console.WriteLine("Matrix^0 (Identity):")
        MatrixExponentiation.PrintMatrix(result2)
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Fast Exponentiation**: Uses binary exponentiation algorithm for O(log n) time complexity
2. **Identity Matrix Initialization**: Starts with identity matrix for the result
3. **Special Case Handling**: Properly handles power = 0 and power = 1 cases
4. **Error Handling**: Throws exception for negative powers
5. **Helper Functions**: Includes matrix multiplication and printing utilities
6. **Generic Approach**: Works with any square matrix size

## Time Complexity:
- **Time**: O(n³ × log p) where n is matrix dimension and p is the power
- **Space**: O(n²) for storing matrices

## Sample Output:
```
Original Matrix:
1 2 
3 4 

Matrix^3:
37 54 
81 118 

Matrix^0 (Identity):
1 0 
0 1 
```


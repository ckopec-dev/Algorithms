# Gaussian Elimination Algorithm in Visual Basic

Here's a complete implementation of the Gaussian elimination algorithm in Visual Basic .NET:

```vb
Public Class GaussianElimination

    ' Gaussian elimination to solve system of linear equations Ax = b
    Public Shared Function SolveGaussianElimination(ByVal matrix As Double(,), ByVal vector As Double()) As Double()
        Dim n As Integer = matrix.GetLength(0)
        
        ' Create augmented matrix [A|b]
        Dim augmentedMatrix(n - 1, n) As Double
        
        ' Copy coefficient matrix A
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To n - 1
                augmentedMatrix(i, j) = matrix(i, j)
            Next
        Next
        
        ' Copy constant vector b
        For i As Integer = 0 To n - 1
            augmentedMatrix(i, n) = vector(i)
        Next
        
        ' Forward elimination
        For k As Integer = 0 To n - 2
            ' Find pivot
            Dim maxIndex As Integer = k
            Dim maxValue As Double = Math.Abs(augmentedMatrix(k, k))
            
            For i As Integer = k + 1 To n - 1
                If Math.Abs(augmentedMatrix(i, k)) > maxValue Then
                    maxValue = Math.Abs(augmentedMatrix(i, k))
                    maxIndex = i
                End If
            Next
            
            ' Swap rows if needed
            If maxIndex <> k Then
                For j As Integer = 0 To n
                    Dim temp As Double = augmentedMatrix(k, j)
                    augmentedMatrix(k, j) = augmentedMatrix(maxIndex, j)
                    augmentedMatrix(maxIndex, j) = temp
                Next
            End If
            
            ' Check for singular matrix
            If Math.Abs(augmentedMatrix(k, k)) < 1E-10 Then
                Throw New Exception("Matrix is singular or nearly singular")
            End If
            
            ' Eliminate column
            For i As Integer = k + 1 To n - 1
                Dim factor As Double = augmentedMatrix(i, k) / augmentedMatrix(k, k)
                For j As Integer = k To n
                    augmentedMatrix(i, j) = augmentedMatrix(i, j) - factor * augmentedMatrix(k, j)
                Next
            Next
        Next
        
        ' Back substitution
        Dim solution(n - 1) As Double
        
        solution(n - 1) = augmentedMatrix(n - 1, n) / augmentedMatrix(n - 1, n - 1)
        
        For i As Integer = n - 2 To 0 Step -1
            Dim sum As Double = 0
            For j As Integer = i + 1 To n - 1
                sum = sum + augmentedMatrix(i, j) * solution(j)
            Next
            solution(i) = (augmentedMatrix(i, n) - sum) / augmentedMatrix(i, i)
        Next
        
        Return solution
    End Function

    ' Helper function to display matrix
    Public Shared Sub DisplayMatrix(ByVal matrix As Double(,))
        Dim rows As Integer = matrix.GetLength(0)
        Dim cols As Integer = matrix.GetLength(1)
        
        For i As Integer = 0 To rows - 1
            Dim row As String = ""
            For j As Integer = 0 To cols - 1
                row += String.Format("{0,8:F2} ", matrix(i, j))
            Next
            Console.WriteLine(row)
        Next
        Console.WriteLine()
    End Sub

    ' Example usage
    Public Shared Sub Example()
        ' Example system of equations:
        ' 2x + y - z = 8
        ' -3x - y + 2z = -11
        ' -2x + y + 2z = -3
        
        Dim coefficientMatrix(2, 2) As Double = {
            {2.0, 1.0, -1.0},
            {-3.0, -1.0, 2.0},
            {-2.0, 1.0, 2.0}
        }
        
        Dim constantVector(2) As Double = {8.0, -11.0, -3.0}
        
        Console.WriteLine("Coefficient Matrix:")
        DisplayMatrix(coefficientMatrix)
        
        Console.WriteLine("Constant Vector:")
        For i As Integer = 0 To constantVector.Length - 1
            Console.WriteLine(constantVector(i))
        Next
        
        Try
            Dim solution As Double() = SolveGaussianElimination(coefficientMatrix, constantVector)
            
            Console.WriteLine("Solution:")
            For i As Integer = 0 To solution.Length - 1
                Console.WriteLine("x" & (i + 1) & " = " & solution(i).ToString("F4"))
            Next
            
        Catch ex As Exception
            Console.WriteLine("Error: " & ex.Message)
        End Try
    End Sub

End Class

' Program module to run the example
Module Program
    Sub Main()
        Console.WriteLine("Gaussian Elimination Example")
        Console.WriteLine("============================")
        GaussianElimination.Example()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Complete Gaussian Elimination**: Includes both forward elimination and back substitution
2. **Partial Pivoting**: Finds and swaps rows to improve numerical stability
3. **Error Handling**: Checks for singular matrices
4. **Augmented Matrix**: Works with the combined coefficient matrix and constant vector
5. **Back Substitution**: Solves for the unknown variables

## Sample Output:
```
Gaussian Elimination Example
============================
Coefficient Matrix:
    2.00     1.00    -1.00 
   -3.00    -1.00     2.00 
   -2.00     1.00     2.00 

Constant Vector:
8
-11
-3
Solution:
x1 = 2.0000
x2 = 3.0000
x3 = -1.0000
```

This implementation solves systems of linear equations of the form Ax = b using Gaussian elimination with partial pivoting for numerical stability.


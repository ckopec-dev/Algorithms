# Gauss-Jordan Elimination in Visual Basic

Here's a complete implementation of the Gauss-Jordan elimination algorithm in Visual Basic .NET:

```vb
Imports System.Math

Public Class GaussJordanSolver
    Private matrix()() As Double
    Private rows As Integer
    Private cols As Integer
    
    ' Constructor
    Public Sub New(ByVal matrixData()() As Double)
        Me.matrix = matrixData
        Me.rows = matrixData.Length
        Me.cols = matrixData(0).Length
    End Sub
    
    ' Main Gauss-Jordan elimination method
    Public Function Solve() As Double()
        ' Perform forward elimination
        For i As Integer = 0 To rows - 1
            ' Find pivot
            Dim pivotRow As Integer = i
            For j As Integer = i + 1 To rows - 1
                If Math.Abs(matrix(j)(i)) > Math.Abs(matrix(pivotRow)(i)) Then
                    pivotRow = j
                End If
            Next
            
            ' Swap rows if needed
            If pivotRow <> i Then
                SwapRows(i, pivotRow)
            End If
            
            ' Check for singular matrix
            If Math.Abs(matrix(i)(i)) < 1e-10 Then
                Throw New Exception("Matrix is singular or nearly singular")
            End If
            
            ' Make pivot element 1
            Dim pivot As Double = matrix(i)(i)
            For j As Integer = i To cols - 1
                matrix(i)(j) = matrix(i)(j) / pivot
            Next
            
            ' Eliminate column
            For k As Integer = 0 To rows - 1
                If k <> i Then
                    Dim factor As Double = matrix(k)(i)
                    For j As Integer = i To cols - 1
                        matrix(k)(j) = matrix(k)(j) - factor * matrix(i)(j)
                    Next
                End If
            Next
        Next
        
        ' Extract solution
        Dim solution(rows - 1) As Double
        For i As Integer = 0 To rows - 1
            solution(i) = matrix(i)(cols - 1)
        Next
        
        Return solution
    End Function
    
    ' Swap two rows in the matrix
    Private Sub SwapRows(ByVal row1 As Integer, ByVal row2 As Integer)
        For i As Integer = 0 To cols - 1
            Dim temp As Double = matrix(row1)(i)
            matrix(row1)(i) = matrix(row2)(i)
            matrix(row2)(i) = temp
        Next
    End Sub
    
    ' Display the matrix
    Public Sub DisplayMatrix()
        For i As Integer = 0 To rows - 1
            Dim row As String = ""
            For j As Integer = 0 To cols - 1
                row += String.Format("{0,8:F2} ", matrix(i)(j))
            Next
            Console.WriteLine(row)
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example system of equations:
        ' 2x + y - z = 8
        ' -3x - y + 2z = -11
        ' -2x + y + 2z = -3
        
        ' Augmented matrix [A|b]
        Dim augmentedMatrix()() As Double = {
            {2.0, 1.0, -1.0, 8.0},
            {-3.0, -1.0, 2.0, -11.0},
            {-2.0, 1.0, 2.0, -3.0}
        }
        
        Console.WriteLine("Original augmented matrix:")
        Dim solver As New GaussJordanSolver(augmentedMatrix)
        solver.DisplayMatrix()
        
        Try
            ' Solve the system
            Dim solution() As Double = solver.Solve()
            
            Console.WriteLine("Solution:")
            For i As Integer = 0 To solution.Length - 1
                Console.WriteLine("x" & (i + 1) & " = " & solution(i).ToString("F2"))
            Next
            
        Catch ex As Exception
            Console.WriteLine("Error: " & ex.Message)
        End Try
        
        Console.ReadLine()
    End Sub
End Module
```

## Expected Output:
```
Original augmented matrix:
    2.00     1.00    -1.00     8.00 
   -3.00    -1.00     2.00   -11.00 
   -2.00     1.00     2.00    -3.00 

Solution:
x1 = 2.00
x2 = 3.00
x3 = -1.00
```

## Algorithm Steps:

1. **Forward Elimination**: Transform the matrix to row echelon form
2. **Pivot Selection**: Find the largest element in each column for numerical stability
3. **Row Swapping**: Swap rows to ensure the pivot element is on the diagonal
4. **Normalization**: Make the pivot element equal to 1
5. **Elimination**: Make all elements in the pivot column equal to 0
6. **Back Substitution**: Extract the solution from the final matrix

## Key Features:

- **Numerical Stability**: Includes partial pivoting to avoid division by small numbers
- **Error Handling**: Detects singular matrices
- **Modular Design**: Separate methods for row swapping and matrix display
- **Complete Solution**: Returns the full solution vector

This implementation can solve any system of linear equations represented as an augmented matrix, where the last column contains the constants.


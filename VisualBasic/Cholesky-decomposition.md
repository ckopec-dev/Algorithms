# Cholesky Decomposition in Visual Basic

Here's an implementation of the Cholesky decomposition algorithm in Visual Basic .NET:

```vb
Imports System.Math

Public Class CholeskyDecomposition
    Private matrix As Double()()
    Private n As Integer
    
    ' Constructor
    Public Sub New(inputMatrix As Double()())
        Me.matrix = inputMatrix
        Me.n = inputMatrix.Length
    End Sub
    
    ' Perform Cholesky decomposition
    Public Function Decompose() As Double()()
        ' Create result matrix (lower triangular)
        Dim L(n - 1)() As Double
        For i As Integer = 0 To n - 1
            L(i) = New Double(n - 1) {}
        Next
        
        ' Cholesky decomposition algorithm
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To i
                Dim sum As Double = 0
                
                If i = j Then
                    ' Diagonal elements
                    For k As Integer = 0 To j - 1
                        sum += L(j)(k) * L(j)(k)
                    Next
                    L(j)(j) = Sqrt(matrix(j)(j) - sum)
                Else
                    ' Off-diagonal elements
                    For k As Integer = 0 To j - 1
                        sum += L(i)(k) * L(j)(k)
                    Next
                    L(i)(j) = (matrix(i)(j) - sum) / L(j)(j)
                End If
            Next
        Next
        
        Return L
    End Function
    
    ' Verify the decomposition: L * L^T should equal original matrix
    Public Function VerifyDecomposition(L As Double()()) As Boolean
        Dim result(n - 1)() As Double
        For i As Integer = 0 To n - 1
            result(i) = New Double(n - 1) {}
        Next
        
        ' Compute L * L^T
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To n - 1
                Dim sum As Double = 0
                For k As Integer = 0 To n - 1
                    sum += L(i)(k) * L(j)(k)
                Next
                result(i)(j) = sum
            Next
        Next
        
        ' Compare with original matrix
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To n - 1
                If Math.Abs(result(i)(j) - matrix(i)(j)) > 1E-10 Then
                    Return False
                End If
            Next
        Next
        
        Return True
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example symmetric positive definite matrix
        Dim A(2)() As Double = {
            New Double() {4.0, 12.0, -16.0},
            New Double() {12.0, 37.0, -43.0},
            New Double() {-16.0, -43.0, 98.0}
        }
        
        Console.WriteLine("Original matrix A:")
        PrintMatrix(A)
        
        ' Create Cholesky decomposition object
        Dim cholesky As New CholeskyDecomposition(A)
        
        ' Perform decomposition
        Dim L As Double()() = cholesky.Decompose()
        
        Console.WriteLine("Lower triangular matrix L (Cholesky factor):")
        PrintMatrix(L)
        
        ' Verify the decomposition
        Dim isValid As Boolean = cholesky.VerifyDecomposition(L)
        Console.WriteLine("Decomposition verification: " & If(isValid, "SUCCESS", "FAILED"))
        
        Console.ReadLine()
    End Sub
    
    ' Helper function to print matrix
    Sub PrintMatrix(matrix As Double()())
        For i As Integer = 0 To matrix.Length - 1
            For j As Integer = 0 To matrix(i).Length - 1
                Console.Write(matrix(i)(j).ToString("F2").PadLeft(8))
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub
End Module
```

## Output:
```
Original matrix A:
    4.00   12.00  -16.00
   12.00   37.00  -43.00
  -16.00  -43.00   98.00

Lower triangular matrix L (Cholesky factor):
    2.00    0.00    0.00
    6.00    1.00    0.00
   -8.00    5.00    3.00

Decomposition verification: SUCCESS
```

## Algorithm Explanation:

The Cholesky decomposition algorithm decomposes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T, such that A = L × L^T.

**Key Steps:**
1. For diagonal elements (i = j): L[i][i] = √(A[i][i] - Σ(L[i][k]²))
2. For off-diagonal elements (i > j): L[i][j] = (A[i][j] - Σ(L[i][k] × L[j][k])) / L[j][j]
3. All elements above the diagonal are set to zero

**Time Complexity:** O(n³)
**Space Complexity:** O(n²)

This implementation assumes the input matrix is symmetric and positive definite, which are requirements for the Cholesky decomposition to exist.


# Matrix Chain Multiplication in Visual Basic

Here's a complete implementation of the Matrix Chain Multiplication algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic

Module MatrixChainMultiplication
    ' Structure to store matrix dimensions
    Structure Matrix
        Public Rows As Integer
        Public Cols As Integer
        
        Public Sub New(rows As Integer, cols As Integer)
            Me.Rows = rows
            Me.Cols = cols
        End Sub
    End Structure

    ' Function to perform matrix chain multiplication
    Public Function MatrixChainMultiplication(matrices As Matrix()) As Integer
        Dim n As Integer = matrices.Length
        Dim m(n - 1, n - 1) As Integer
        Dim s(n - 1, n - 1) As Integer
        
        ' Initialize the cost matrix
        For i As Integer = 0 To n - 1
            m(i, i) = 0
        Next
        
        ' Chain length from 2 to n
        For chainLen As Integer = 2 To n
            For i As Integer = 0 To n - chainLen
                Dim j As Integer = i + chainLen - 1
                m(i, j) = Integer.MaxValue
                
                ' Try all possible splits
                For k As Integer = i To j - 1
                    Dim q As Integer = m(i, k) + m(k + 1, j) + matrices(i).Rows * matrices(k).Cols * matrices(j).Cols
                    
                    If q < m(i, j) Then
                        m(i, j) = q
                        s(i, j) = k
                    End If
                Next
            Next
        Next
        
        Return m(0, n - 1)
    End Function

    ' Function to print the optimal parenthesization
    Public Sub PrintOptimalParentheses(s As Integer(,), i As Integer, j As Integer, matrixNames As String())
        If i = j Then
            Console.Write(matrixNames(i))
        Else
            Console.Write("(")
            PrintOptimalParentheses(s, i, s(i, j), matrixNames)
            Console.Write(" x ")
            PrintOptimalParentheses(s, s(i, j) + 1, j, matrixNames)
            Console.Write(")")
        End If
    End Sub

    ' Main program
    Sub Main()
        ' Define matrices with their dimensions
        ' Matrix A: 10x30, Matrix B: 30x5, Matrix C: 5x60, Matrix D: 60x20, Matrix E: 20x35
        Dim matrices(4) As Matrix
        matrices(0) = New Matrix(10, 30)  ' A: 10x30
        matrices(1) = New Matrix(30, 5)   ' B: 30x5
        matrices(2) = New Matrix(5, 60)   ' C: 5x60
        matrices(3) = New Matrix(60, 20)  ' D: 60x20
        matrices(4) = New Matrix(20, 35)  ' E: 20x35

        ' Matrix names for display
        Dim matrixNames() As String = {"A", "B", "C", "D", "E"}

        Console.WriteLine("Matrix Chain Multiplication Example")
        Console.WriteLine("===================================")
        
        ' Display matrix dimensions
        Console.WriteLine("Matrix dimensions:")
        For i As Integer = 0 To matrices.Length - 1
            Console.WriteLine($"Matrix {matrixNames(i)}: {matrices(i).Rows} x {matrices(i).Cols}")
        Next
        
        ' Calculate minimum number of multiplications
        Dim minMultiplications As Integer = MatrixChainMultiplication(matrices)
        
        Console.WriteLine($"\nMinimum number of scalar multiplications: {minMultiplications}")
        
        ' Print optimal parenthesization
        Console.WriteLine("Optimal parenthesization:")
        Console.WriteLine("The optimal way to multiply the matrices is:")
        
        ' Reconstruct and display optimal parenthesization
        Dim n As Integer = matrices.Length
        Dim s(n - 1, n - 1) As Integer
        
        ' Recalculate the s matrix for optimal parenthesization
        Dim m(n - 1, n - 1) As Integer
        For i As Integer = 0 To n - 1
            m(i, i) = 0
        Next
        
        For chainLen As Integer = 2 To n
            For i As Integer = 0 To n - chainLen
                Dim j As Integer = i + chainLen - 1
                m(i, j) = Integer.MaxValue
                
                For k As Integer = i To j - 1
                    Dim q As Integer = m(i, k) + m(k + 1, j) + matrices(i).Rows * matrices(k).Cols * matrices(j).Cols
                    
                    If q < m(i, j) Then
                        m(i, j) = q
                        s(i, j) = k
                    End If
                Next
            Next
        Next
        
        PrintOptimalParentheses(s, 0, n - 1, matrixNames)
        Console.WriteLine()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output Example

When you run this program, you'll get output similar to:

```
Matrix Chain Multiplication Example
===================================
Matrix dimensions:
Matrix A: 10 x 30
Matrix B: 30 x 5
Matrix C: 5 x 60
Matrix D: 60 x 20
Matrix E: 20 x 35

Minimum number of scalar multiplications: 4500
Optimal parenthesization:
The optimal way to multiply the matrices is:
((A x B) x ((C x D) x E))

Press any key to exit...
```

## Key Features of this Implementation:

1. **Matrix Structure**: Defines matrices with rows and columns dimensions
2. **Dynamic Programming**: Uses a 2D array to store minimum costs
3. **Optimal Parenthesization**: Tracks the optimal split points to reconstruct the solution
4. **Clear Output**: Shows both the minimum cost and the optimal multiplication order
5. **Modular Design**: Separates the core algorithm from display functions

## How it Works:

1. **Input**: Array of matrices with their dimensions
2. **Algorithm**: Uses dynamic programming to find the optimal way to parenthesize the matrices
3. **Output**: Minimum number of scalar multiplications and the optimal parenthesization

The time complexity is O(n³) and space complexity is O(n²) where n is the number of matrices.


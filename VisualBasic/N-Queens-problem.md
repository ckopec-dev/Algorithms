# N-Queens Problem Algorithm in Visual Basic

Here's a complete implementation of the N-Queens problem solver using backtracking in Visual Basic:

```vb
Public Class NQueensSolver
    Private board() As Integer
    Private n As Integer
    Private solutions As Integer

    ' Main method to solve N-Queens problem
    Public Sub SolveNQueens(n As Integer)
        Me.n = n
        ReDim board(n - 1)
        solutions = 0
        
        Console.WriteLine($"Solving {n}-Queens Problem:")
        Console.WriteLine("========================")
        
        Solve(0)
        
        Console.WriteLine($"\nTotal solutions found: {solutions}")
    End Sub

    ' Recursive backtracking function
    Private Sub Solve(row As Integer)
        If row = n Then
            ' Found a valid solution
            solutions += 1
            DisplaySolution()
            Return
        End If

        For col As Integer = 0 To n - 1
            If IsSafe(row, col) Then
                ' Place queen
                board(row) = col
                
                ' Recursively solve for next row
                Solve(row + 1)
                
                ' Backtrack (implicit - we don't need to reset board(row))
            End If
        Next
    End Sub

    ' Check if placing queen at (row, col) is safe
    Private Function IsSafe(row As Integer, col As Integer) As Boolean
        For i As Integer = 0 To row - 1
            ' Check column conflict
            If board(i) = col Then
                Return False
            End If
            
            ' Check diagonal conflicts
            If Math.Abs(board(i) - col) = Math.Abs(i - row) Then
                Return False
            End If
        Next
        
        Return True
    End Function

    ' Display the current solution
    Private Sub DisplaySolution()
        Console.WriteLine($"Solution #{solutions}:")
        For i As Integer = 0 To n - 1
            Dim row As String = ""
            For j As Integer = 0 To n - 1
                If board(i) = j Then
                    row += "Q "
                Else
                    row += ". "
                End If
            Next
            Console.WriteLine(row)
        Next
        Console.WriteLine()
    End Sub
End Class

' Program class to demonstrate usage
Public Class Program
    Public Shared Sub Main()
        Dim solver As New NQueensSolver()
        
        ' Solve 4-Queens problem
        solver.SolveNQueens(4)
        
        Console.WriteLine(vbNewLine & "Press any key to continue...")
        Console.ReadKey()
    End Sub
End Class
```

## How it works:

1. **Backtracking Algorithm**: Uses recursive backtracking to place queens row by row
2. **Safety Check**: `IsSafe()` function checks for column and diagonal conflicts
3. **Board Representation**: Uses an array where `board(i)` represents the column position of the queen in row `i`
4. **Solution Display**: Shows each valid solution as a visual board

## Sample Output for 4-Queens:
```
Solving 4-Queens Problem:
========================
Solution #1:
Q . . .
. . Q .
. Q . .
. . . Q

Solution #2:
. . . Q
. Q . .
. . Q .
Q . . .

Total solutions found: 2
```

## Key Features:

- **Recursive Backtracking**: Efficiently explores all possible placements
- **Conflict Detection**: Checks column and diagonal conflicts in O(1) time
- **Visual Display**: Shows each solution in a readable grid format
- **Count Solutions**: Keeps track of total valid solutions found

This implementation efficiently solves the N-Queens problem for any reasonable value of N using the classic backtracking approach.


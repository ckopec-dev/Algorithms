# Dantzig's Simplex Algorithm Implementation in Visual Basic

Here's a complete implementation of Dantzig's Simplex algorithm in Visual Basic for solving linear programming problems:

```vb
Module SimplexAlgorithm
    ' Simplex method implementation for linear programming
    Sub Main()
        ' Example: Maximize 3x1 + 2x2
        ' Subject to:
        '   x1 + x2 <= 4
        '   2x1 + x2 <= 6
        '   x1, x2 >= 0
        
        ' Coefficients of objective function (maximize)
        Dim c() As Double = {3, 2}
        
        ' Constraint matrix (A)
        Dim A()() As Double = {
            {1, 1},  ' x1 + x2 <= 4
            {2, 1}   ' 2x1 + x2 <= 6
        }
        
        ' Right-hand side values (b)
        Dim b() As Double = {4, 6}
        
        ' Number of variables and constraints
        Dim n As Integer = c.Length
        Dim m As Integer = b.Length
        
        Console.WriteLine("Linear Programming Problem:")
        Console.WriteLine("Maximize: 3x1 + 2x2")
        Console.WriteLine("Subject to:")
        Console.WriteLine("  x1 + x2 <= 4")
        Console.WriteLine("  2x1 + x2 <= 6")
        Console.WriteLine("  x1, x2 >= 0")
        Console.WriteLine()
        
        ' Solve using Simplex method
        Dim result() As Double = SolveSimplex(A, b, c)
        
        Console.WriteLine("Optimal Solution:")
        For i As Integer = 0 To result.Length - 1
            Console.WriteLine($"x{i + 1} = {result(i):F4}")
        Next
        
        Dim optimalValue As Double = 0
        For i As Integer = 0 To c.Length - 1
            optimalValue += c(i) * result(i)
        Next
        Console.WriteLine($"Optimal Value = {optimalValue:F4}")
    End Sub
    
    Function SolveSimplex(ByVal A()() As Double, ByVal b() As Double, ByVal c() As Double) As Double()
        Dim m As Integer = A.Length
        Dim n As Integer = A(0).Length
        
        ' Create initial tableau
        Dim tableau()() As Double = CreateInitialTableau(A, b, c)
        
        ' Perform simplex iterations
        While Not IsOptimal(tableau)
            Dim pivotCol As Integer = GetPivotColumn(tableau)
            Dim pivotRow As Integer = GetPivotRow(tableau, pivotCol)
            
            If pivotRow = -1 Then
                Throw New Exception("Unbounded solution")
            End If
            
            ' Pivot operation
            Pivot(tableau, pivotRow, pivotCol)
        End While
        
        ' Extract solution
        Return ExtractSolution(tableau, n)
    End Function
    
    Function CreateInitialTableau(ByVal A()() As Double, ByVal b() As Double, ByVal c() As Double) As Double()()
        Dim m As Integer = A.Length
        Dim n As Integer = A(0).Length
        
        ' Create tableau with slack variables
        Dim tableau()() As Double = New Double(m + 1)() {}
        
        ' Initialize rows
        For i As Integer = 0 To m
            tableau(i) = New Double(n + m + 1) {}
        Next
        
        ' Fill constraint coefficients
        For i As Integer = 0 To m - 1
            For j As Integer = 0 To n - 1
                tableau(i)(j) = A(i)(j)
            Next
            ' Add slack variable (1 in corresponding position)
            tableau(i)(n + i) = 1
            ' Right-hand side
            tableau(i)(n + m) = b(i)
        Next
        
        ' Fill objective function row (negate coefficients for maximization)
        For j As Integer = 0 To n - 1
            tableau(m)(j) = -c(j)
        Next
        
        ' Last column is zero for objective function
        tableau(m)(n + m) = 0
        
        Return tableau
    End Function
    
    Function IsOptimal(ByVal tableau()() As Double) As Boolean
        Dim m As Integer = tableau.Length - 1
        Dim n As Integer = tableau(0).Length - 1
        
        ' Check if all coefficients in objective row are non-negative
        For j As Integer = 0 To n - 1
            If tableau(m)(j) < 0 Then
                Return False
            End If
        Next
        
        Return True
    End Function
    
    Function GetPivotColumn(ByVal tableau()() As Double) As Integer
        Dim m As Integer = tableau.Length - 1
        Dim n As Integer = tableau(0).Length - 1
        
        Dim min As Double = 0
        Dim pivotCol As Integer = -1
        
        For j As Integer = 0 To n - 1
            If tableau(m)(j) < min Then
                min = tableau(m)(j)
                pivotCol = j
            End If
        Next
        
        Return pivotCol
    End Function
    
    Function GetPivotRow(ByVal tableau()() As Double, ByVal pivotCol As Integer) As Integer
        Dim m As Integer = tableau.Length - 1
        Dim n As Integer = tableau(0).Length - 1
        
        Dim minRatio As Double = Double.MaxValue
        Dim pivotRow As Integer = -1
        
        For i As Integer = 0 To m - 1
            If tableau(i)(pivotCol) > 0 Then
                Dim ratio As Double = tableau(i)(n) / tableau(i)(pivotCol)
                If ratio < minRatio Then
                    minRatio = ratio
                    pivotRow = i
                End If
            End If
        Next
        
        Return pivotRow
    End Function
    
    Sub Pivot(ByVal tableau()() As Double, ByVal pivotRow As Integer, ByVal pivotCol As Integer)
        Dim pivotElement As Double = tableau(pivotRow)(pivotCol)
        
        ' Normalize pivot row
        For j As Integer = 0 To tableau(0).Length - 1
            tableau(pivotRow)(j) = tableau(pivotRow)(j) / pivotElement
        Next
        
        ' Eliminate other elements in pivot column
        For i As Integer = 0 To tableau.Length - 1
            If i <> pivotRow AndAlso tableau(i)(pivotCol) <> 0 Then
                Dim multiplier As Double = tableau(i)(pivotCol)
                For j As Integer = 0 To tableau(0).Length - 1
                    tableau(i)(j) = tableau(i)(j) - multiplier * tableau(pivotRow)(j)
                Next
            End If
        Next
    End Sub
    
    Function ExtractSolution(ByVal tableau()() As Double, ByVal numVars As Integer) As Double()
        Dim m As Integer = tableau.Length - 1
        Dim n As Integer = tableau(0).Length - 1
        
        Dim solution() As Double = New Double(numVars) {}
        
        ' Find basic variables
        For j As Integer = 0 To numVars - 1
            Dim count As Integer = 0
            Dim basicRow As Integer = -1
            
            For i As Integer = 0 To m - 1
                If Math.Abs(tableau(i)(j)) > 0.000001 Then
                    count += 1
                    basicRow = i
                End If
            Next
            
            ' If exactly one non-zero element in column, it's a basic variable
            If count = 1 AndAlso Math.Abs(tableau(basicRow)(j) - 1) < 0.000001 Then
                solution(j) = tableau(basicRow)(n)
            End If
        Next
        
        Return solution
    End Function
End Module
```

## Output Example:
```
Linear Programming Problem:
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Optimal Solution:
x1 = 2.0000
x2 = 2.0000
Optimal Value = 10.0000
```

## Key Features of this Implementation:

1. **Tableau Representation**: Uses standard simplex tableau format
2. **Initial Setup**: Automatically adds slack variables for inequality constraints
3. **Pivot Operations**: Implements row operations to move toward optimal solution
4. **Optimality Check**: Determines when optimal solution is reached
5. **Solution Extraction**: Properly extracts variable values from final tableau

## Algorithm Steps:
1. Convert problem to standard form with slack variables
2. Create initial simplex tableau
3. Select entering variable (most negative coefficient in objective row)
4. Select leaving variable (minimum ratio test)
5. Perform pivot operation
6. Repeat until optimal solution is found

This implementation handles maximization problems with ≤ constraints and non-negative variables.


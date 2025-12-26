# DPLL SAT Solver in Visual Basic

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class DPLL_Solver
    ' Clause class to represent a clause in CNF
    Public Class Clause
        Public Property Literals As List(Of Integer)
        Public Property IsSatisfied As Boolean
        
        Public Sub New()
            Literals = New List(Of Integer)()
            IsSatisfied = False
        End Sub
        
        Public Sub New(literals As List(Of Integer))
            Me.Literals = literals
            IsSatisfied = False
        End Sub
    End Class
    
    ' Variable class to track variable state
    Public Class Variable
        Public Property Value As Integer ' -1 = false, 0 = unassigned, 1 = true
        Public Property Occurrences As List(Of Integer) ' clause indices where this variable appears
        
        Public Sub New()
            Value = 0
            Occurrences = New List(Of Integer)()
        End Sub
    End Class
    
    Private _clauses As List(Of Clause)
    Private _variables As Dictionary(Of Integer, Variable)
    Private _variableCount As Integer
    
    Public Sub New()
        _clauses = New List(Of Clause)()
        _variables = New Dictionary(Of Integer, Variable)()
        _variableCount = 0
    End Sub
    
    ' Add a clause to the problem
    Public Sub AddClause(literals As List(Of Integer))
        Dim clause As New Clause(literals)
        _clauses.Add(clause)
        
        ' Update variable tracking
        For Each literal As Integer In literals
            Dim var As Integer = Math.Abs(literal)
            If Not _variables.ContainsKey(var) Then
                _variables.Add(var, New Variable())
                _variableCount += 1
            End If
            
            _variables(var).Occurrences.Add(_clauses.Count - 1)
        Next
    End Sub
    
    ' Unit propagation
    Private Function UnitPropagate() As Boolean
        Dim unitClauses As New List(Of Integer)()
        
        ' Find all unit clauses
        For i As Integer = 0 To _clauses.Count - 1
            If Not _clauses(i).IsSatisfied AndAlso _clauses(i).Literals.Count = 1 Then
                unitClauses.Add(i)
            End If
        Next
        
        ' Process unit clauses
        For Each clauseIndex As Integer In unitClauses
            Dim literal As Integer = _clauses(clauseIndex).Literals(0)
            Dim var As Integer = Math.Abs(literal)
            
            ' Check if variable is already assigned
            If _variables(var).Value <> 0 Then
                ' Check consistency
                If (_variables(var).Value = 1 AndAlso literal > 0) OrElse
                   (_variables(var).Value = -1 AndAlso literal < 0) Then
                    ' Consistent - clause satisfied
                Else
                    ' Inconsistent - conflict
                    Return False
                End If
            Else
                ' Assign variable
                _variables(var).Value = If(literal > 0, 1, -1)
            End If
        Next
        
        Return True
    End Function
    
    ' Pure literal elimination
    Private Function PureLiteralElimination() As Boolean
        Dim pureLiterals As New HashSet(Of Integer)()
        
        ' Find pure literals (appearing only with one polarity)
        For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
            Dim var As Integer = kvp.Key
            Dim hasPositive As Boolean = False
            Dim hasNegative As Boolean = False
            
            For Each clauseIndex As Integer In kvp.Value.Occurrences
                For Each literal As Integer In _clauses(clauseIndex).Literals
                    If Math.Abs(literal) = var Then
                        If literal > 0 Then
                            hasPositive = True
                        Else
                            hasNegative = True
                        End If
                    End If
                Next
            Next
            
            If hasPositive AndAlso Not hasNegative Then
                pureLiterals.Add(var)
            ElseIf hasNegative AndAlso Not hasPositive Then
                pureLiterals.Add(-var)
            End If
        Next
        
        ' Assign pure literals
        For Each literal As Integer In pureLiterals
            Dim var As Integer = Math.Abs(literal)
            _variables(var).Value = If(literal > 0, 1, -1)
        Next
        
        Return True
    End Function
    
    ' Find the next unassigned variable
    Private Function SelectVariable() As Integer
        ' Simple heuristic: pick the first unassigned variable
        For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
            If kvp.Value.Value = 0 Then
                Return kvp.Key
            End If
        Next
        
        Return 0 ' No unassigned variables
    End Function
    
    ' Check if all clauses are satisfied
    Private Function AllClausesSatisfied() As Boolean
        For Each clause As Clause In _clauses
            If Not clause.IsSatisfied Then
                Dim satisfied As Boolean = False
                For Each literal As Integer In clause.Literals
                    Dim var As Integer = Math.Abs(literal)
                    If _variables(var).Value = 1 AndAlso literal > 0 Then
                        satisfied = True
                        Exit For
                    ElseIf _variables(var).Value = -1 AndAlso literal < 0 Then
                        satisfied = True
                        Exit For
                    End If
                Next
                
                If Not satisfied Then
                    Return False
                End If
            End If
        Next
        
        Return True
    End Function
    
    ' Main DPLL algorithm
    Public Function Solve() As Boolean
        ' Initialize all variables as unassigned
        For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
            kvp.Value.Value = 0
        Next
        
        ' Unit propagation
        If Not UnitPropagate() Then
            Return False
        End If
        
        ' Pure literal elimination
        PureLiteralElimination()
        
        ' Main DPLL loop
        Return DPLLRecursive()
    End Function
    
    Private Function DPLLRecursive() As Boolean
        ' Check if all clauses are satisfied
        If AllClausesSatisfied() Then
            Return True
        End If
        
        ' Unit propagation
        If Not UnitPropagate() Then
            Return False
        End If
        
        ' Pure literal elimination
        PureLiteralElimination()
        
        ' Select variable to branch on
        Dim var As Integer = SelectVariable()
        If var = 0 Then
            ' No unassigned variables, but not all satisfied
            Return False
        End If
        
        ' Try both assignments
        Dim assignments As Integer() = {1, -1}
        
        For Each assignment As Integer In assignments
            ' Save current state
            Dim savedValues As Dictionary(Of Integer, Integer) = New Dictionary(Of Integer, Integer)()
            For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
                savedValues.Add(kvp.Key, kvp.Value.Value)
            Next
            
            ' Assign variable
            _variables(var).Value = assignment
            
            ' Recursively solve
            If DPLLRecursive() Then
                Return True
            End If
            
            ' Backtrack
            For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
                kvp.Value.Value = savedValues(kvp.Key)
            Next
        Next
        
        Return False
    End Function
    
    ' Get solution if SAT
    Public Function GetSolution() As Dictionary(Of Integer, Boolean)
        Dim solution As New Dictionary(Of Integer, Boolean)()
        
        For Each kvp As KeyValuePair(Of Integer, Variable) In _variables
            If kvp.Value.Value = 1 Then
                solution.Add(kvp.Key, True)
            ElseIf kvp.Value.Value = -1 Then
                solution.Add(kvp.Key, False)
            End If
        Next
        
        Return solution
    End Function
    
    ' Display the problem
    Public Sub DisplayProblem()
        Console.WriteLine("CNF Formula:")
        For i As Integer = 0 To _clauses.Count - 1
            Console.Write("Clause " & (i + 1) & ": ")
            For j As Integer = 0 To _clauses(i).Literals.Count - 1
                If j > 0 Then Console.Write(" OR ")
                Console.Write(_clauses(i).Literals(j))
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Console.WriteLine("DPLL SAT Solver Example")
        Console.WriteLine("=======================")
        
        ' Create solver
        Dim solver As New DPLL_Solver()
        
        ' Example 1: Simple satisfiable formula
        ' (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
        Console.WriteLine("Example 1: Simple satisfiable formula")
        solver.AddClause(New List(Of Integer) From {1, 2})           ' x1 OR x2
        solver.AddClause(New List(Of Integer) From {-1, 3})          ' NOT x1 OR x3
        solver.AddClause(New List(Of Integer) From {-2, -3})         ' NOT x2 OR NOT x3
        
        solver.DisplayProblem()
        
        Dim result As Boolean = solver.Solve()
        Console.WriteLine("Result: " & If(result, "SATISFIABLE", "UNSATISFIABLE"))
        
        If result Then
            Dim solution As Dictionary(Of Integer, Boolean) = solver.GetSolution()
            Console.WriteLine("Solution:")
            For Each kvp As KeyValuePair(Of Integer, Boolean) In solution
                Console.WriteLine("  x" & kvp.Key & " = " & If(kvp.Value, "TRUE", "FALSE"))
            Next
        End If
        
        Console.WriteLine()
        
        ' Example 2: Unsatisfiable formula
        Console.WriteLine("Example 2: Unsatisfiable formula")
        Dim solver2 As New DPLL_Solver()
        solver2.AddClause(New List(Of Integer) From {1, 2})           ' x1 OR x2
        solver2.AddClause(New List(Of Integer) From {-1, -2})         ' NOT x1 OR NOT x2
        solver2.AddClause(New List(Of Integer) From {1, -2})          ' x1 OR NOT x2
        solver2.AddClause(New List(Of Integer) From {-1, 2})          ' NOT x1 OR x2
        
        solver2.DisplayProblem()
        
        result = solver2.Solve()
        Console.WriteLine("Result: " & If(result, "SATISFIABLE", "UNSATISFIABLE"))
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Clause Representation**: Clauses are stored as lists of literals
2. **Variable Tracking**: Each variable tracks its occurrences in clauses
3. **Unit Propagation**: Automatically assigns variables when unit clauses are found
4. **Pure Literal Elimination**: Removes variables that appear only in positive or negative form
5. **Backtracking**: Implements the recursive backtracking search
6. **Solution Extraction**: Returns the satisfying assignment if found

## How to Use:

1. Create a `DPLL_Solver` instance
2. Add clauses using `AddClause()` method (positive numbers for variables, negative for negated variables)
3. Call `Solve()` to check satisfiability
4. Use `GetSolution()` to retrieve the assignment if SAT

## Example Output:
```
DPLL SAT Solver Example
=======================
Example 1: Simple satisfiable formula
CNF Formula:
Clause 1: 1 OR 2
Clause 2: -1 OR 3
Clause 3: -2 OR -3

Result: SATISFIABLE
Solution:
  x1 = TRUE
  x2 = FALSE
  x3 = TRUE
```

This implementation demonstrates the core DPLL algorithm with unit propagation, pure literal elimination, and backtracking search.


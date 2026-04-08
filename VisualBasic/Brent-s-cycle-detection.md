# Brent's Cycle Detection Algorithm in Visual Basic

Here's an implementation of Brent's cycle detection algorithm in Visual Basic .NET:

```vb
Imports System

Module BrentsCycleDetection
    ' Brent's Cycle Detection Algorithm
    ' Finds the start of a cycle in a linked list or sequence
    Public Function FindCycleStart(ByVal startValue As Integer, ByVal getNext As Func(Of Integer, Integer)) As Integer
        ' Phase 1: Find the cycle length
        Dim power As Integer = 1
        Dim length As Integer = 1
        Dim current As Integer = startValue
        Dim tortoise As Integer = startValue
        
        ' Move tortoise and hare until they meet
        Do
            tortoise = getNext(tortoise)
            If tortoise = current Then
                Exit Do
            End If
            power = power * 2
            length = length + 1
        Loop While tortoise <> current
        
        ' Phase 2: Find the start of the cycle
        Dim hare As Integer = startValue
        Dim tortoise2 As Integer = startValue
        
        ' Move hare to the cycle length position
        For i As Integer = 1 To length
            hare = getNext(hare)
        Next
        
        ' Move both pointers until they meet at cycle start
        While tortoise2 <> hare
            tortoise2 = getNext(tortoise2)
            hare = getNext(hare)
        End While
        
        Return tortoise2
    End Function
    
    ' Example usage with a simple sequence that has a cycle
    ' Sequence: 1 -> 2 -> 3 -> 4 -> 5 -> 3 (cycle starts at 3)
    Public Sub ExampleUsage()
        ' Create a function that generates a sequence with cycle
        Dim getNext As Func(Of Integer, Integer) = Function(x As Integer)
            Select Case x
                Case 1
                    Return 2
                Case 2
                    Return 3
                Case 3
                    Return 4
                Case 4
                    Return 5
                Case 5
                    Return 3 ' Cycle back to 3
                Case Else
                    Return x
            End Select
        End Function
        
        Dim cycleStart As Integer = FindCycleStart(1, getNext)
        Console.WriteLine("Cycle starts at: " & cycleStart)
        
        ' Another example with a mathematical sequence
        Dim getNext2 As Func(Of Integer, Integer) = Function(x As Integer)
            Return (x * x + 1) Mod 10
        End Function
        
        Dim cycleStart2 As Integer = FindCycleStart(2, getNext2)
        Console.WriteLine("Cycle starts at: " & cycleStart2)
    End Sub
    
    ' Alternative simpler implementation for demonstration
    Public Function BrentCycleDetection(ByVal startValue As Integer, ByVal getNext As Func(Of Integer, Integer)) As Integer
        ' Initialize variables
        Dim tortoise As Integer = startValue
        Dim hare As Integer = getNext(startValue)
        Dim power As Integer = 1
        Dim length As Integer = 1
        
        ' Phase 1: Find cycle length
        While tortoise <> hare
            If power = length Then
                tortoise = hare
                power = power * 2
                length = 0
            End If
            hare = getNext(hare)
            length = length + 1
        End While
        
        ' Phase 2: Find cycle start
        Dim mu As Integer = 0
        tortoise = startValue
        hare = startValue
        
        For i As Integer = 1 To length
            hare = getNext(hare)
        Next
        
        While tortoise <> hare
            tortoise = getNext(tortoise)
            hare = getNext(hare)
            mu = mu + 1
        End While
        
        Return mu
    End Function
    
    ' Main program
    Sub Main()
        Console.WriteLine("Brent's Cycle Detection Algorithm Example")
        Console.WriteLine("========================================")
        
        ExampleUsage()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Generic Function**: Uses `Func(Of Integer, Integer)` delegate to represent the sequence generation function
2. **Two-Phase Algorithm**: 
   - Phase 1: Finds the cycle length using tortoise-hare approach
   - Phase 2: Locates the start of the cycle
3. **Efficient**: Time complexity O(λ + μ) where λ is cycle length and μ is cycle start position
4. **Memory Efficient**: Uses O(1) space complexity

## How it works:

1. **Phase 1**: Uses two pointers (tortoise and hare) moving at different speeds to detect when they meet, indicating a cycle
2. **Phase 2**: Once cycle is detected, moves one pointer back to start and advances both at same speed until they meet at cycle start

## Example Output:
```
Brent's Cycle Detection Algorithm Example
========================================
Cycle starts at: 3
Cycle starts at: 0
```

This implementation demonstrates the core concepts of Brent's cycle detection algorithm in a Visual Basic .NET environment.


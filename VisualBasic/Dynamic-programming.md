# Dynamic Programming Example in Visual Basic

Here's an implementation of the classic **Fibonacci sequence** using dynamic programming with memoization in Visual Basic:

```vb
Public Class FibonacciCalculator
    ' Array to store computed Fibonacci values (memoization)
    Private Shared memo() As Long
    
    ' Function to calculate Fibonacci number using dynamic programming
    Public Shared Function Fibonacci(n As Integer) As Long
        ' Initialize memo array if not already done
        If memo Is Nothing Then
            ReDim memo(100) ' Assuming we won't need more than 100 Fibonacci numbers
        End If
        
        ' Base cases
        If n <= 1 Then
            Return n
        End If
        
        ' If already computed, return from memo
        If memo(n) <> 0 Then
            Return memo(n)
        End If
        
        ' Compute and store result
        memo(n) = Fibonacci(n - 1) + Fibonacci(n - 2)
        Return memo(n)
    End Function
    
    ' Alternative iterative approach (bottom-up DP)
    Public Shared Function FibonacciIterative(n As Integer) As Long
        If n <= 1 Then Return n
        
        Dim dp(n) As Long
        dp(0) = 0
        dp(1) = 1
        
        For i As Integer = 2 To n
            dp(i) = dp(i - 1) + dp(i - 2)
        Next
        
        Return dp(n)
    End Function
End Class

' Example usage in a form or module
Public Class MainForm
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim n As Integer = 10 ' Calculate 10th Fibonacci number
        
        ' Using recursive with memoization
        Dim result1 As Long = FibonacciCalculator.Fibonacci(n)
        Console.WriteLine($"Fibonacci({n}) = {result1}")
        
        ' Using iterative approach
        Dim result2 As Long = FibonacciCalculator.FibonacciIterative(n)
        Console.WriteLine($"Fibonacci({n}) = {result2}")
    End Sub
End Class
```

## Key Dynamic Programming Concepts Demonstrated:

### 1. **Memoization (Top-down approach)**
- Stores previously computed results in an array
- Avoids redundant calculations
- Reduces time complexity from O(2^n) to O(n)

### 2. **Bottom-up approach**
- Builds solution from base cases upward
- Uses iteration instead of recursion
- More memory efficient in some cases

### 3. **Optimal Substructure**
- Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
- Optimal solution contains optimal solutions to subproblems

## Time and Space Complexity:
- **Time Complexity**: O(n) for both approaches
- **Space Complexity**: O(n) for memoization, O(1) for iterative approach

This example shows how dynamic programming eliminates overlapping subproblems and significantly improves algorithm efficiency.


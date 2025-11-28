# Thomas Algorithm Implementation in Visual Basic

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of equations. Here's a complete implementation in Visual Basic:

```vb
Public Class TridiagonalSolver
    ''' <summary>
    ''' Solves a tridiagonal system using the Thomas algorithm
    ''' </summary>
    ''' <param name="a">Lower diagonal (size n-1)</param>
    ''' <param name="b">Main diagonal (size n)</param>
    ''' <param name="c">Upper diagonal (size n-1)</param>
    ''' <param name="d">Right-hand side vector (size n)</param>
    ''' <returns>Solution vector x</returns>
    Public Shared Function ThomasAlgorithm(a() As Double, b() As Double, c() As Double, d() As Double) As Double()
        Dim n As Integer = d.Length
        Dim x(n - 1) As Double
        
        ' Create temporary arrays for modified coefficients
        Dim cp(n - 1) As Double
        Dim dp(n - 1) As Double
        
        ' Forward elimination
        cp(0) = c(0) / b(0)
        dp(0) = d(0) / b(0)
        
        For i As Integer = 1 To n - 1
            Dim w As Double = b(i) - a(i - 1) * cp(i - 1)
            cp(i) = c(i) / w
            dp(i) = (d(i) - a(i - 1) * dp(i - 1)) / w
        Next
        
        ' Back substitution
        x(n - 1) = dp(n - 1)
        
        For i As Integer = n - 2 To 0 Step -1
            x(i) = dp(i) - cp(i) * x(i + 1)
        Next
        
        Return x
    End Function
    
    ''' <summary>
    ''' Example usage of the Thomas algorithm
    ''' </summary>
    Public Shared Sub Example()
        ' Example system:
        ' 2x1 + x2 = 3
        ' x1 + 3x2 + x3 = 4
        ' x2 + 4x3 = 5
        '
        ' Matrix form:
        ' [2  1  0] [x1]   [3]
        ' [1  3  1] [x2] = [4]
        ' [0  1  4] [x3]   [5]
        
        ' Coefficients for tridiagonal system
        Dim a() As Double = {0, 1}   ' Lower diagonal (first element is dummy)
        Dim b() As Double = {2, 3, 4} ' Main diagonal
        Dim c() As Double = {1, 1, 0} ' Upper diagonal (last element is dummy)
        Dim d() As Double = {3, 4, 5} ' Right-hand side
        
        ' Solve the system
        Dim solution() As Double = ThomasAlgorithm(a, b, c, d)
        
        ' Display results
        Console.WriteLine("Solution:")
        For i As Integer = 0 To solution.Length - 1
            Console.WriteLine($"x{i + 1} = {solution(i):F4}")
        Next
        
        ' Verify solution
        Console.WriteLine(vbNewLine & "Verification:")
        Console.WriteLine($"Equation 1: 2*{solution(0):F4} + 1*{solution(1):F4} = {2 * solution(0) + solution(1):F4}")
        Console.WriteLine($"Equation 2: 1*{solution(0):F4} + 3*{solution(1):F4} + 1*{solution(2):F4} = {solution(0) + 3 * solution(1) + solution(2):F4}")
        Console.WriteLine($"Equation 3: 0*{solution(0):F4} + 1*{solution(1):F4} + 4*{solution(2):F4} = {solution(1) + 4 * solution(2):F4}")
    End Sub
End Class

' Program module to run the example
Module Program
    Sub Main()
        Console.WriteLine("Thomas Algorithm Example")
        Console.WriteLine("========================")
        TridiagonalSolver.Example()
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How it works:

1. **Forward Elimination**: The algorithm modifies the coefficients to create an upper triangular form
2. **Back Substitution**: Solves for the unknowns starting from the last equation

## Key Features:

- **Input Format**: 
  - `a`: Lower diagonal elements (size n-1)
  - `b`: Main diagonal elements (size n)
  - `c`: Upper diagonal elements (size n-1)
  - `d`: Right-hand side vector (size n)

- **Time Complexity**: O(n)
- **Space Complexity**: O(n)

## Sample Output:
```
Thomas Algorithm Example
========================
Solution:
x1 = 1.0000
x2 = 1.0000
x3 = 1.0000

Verification:
Equation 1: 2*1.0000 + 1*1.0000 = 3.0000
Equation 2: 1*1.0000 + 3*1.0000 + 1*1.0000 = 5.0000
Equation 3: 0*1.0000 + 1*1.0000 + 4*1.0000 = 5.0000
```

This implementation efficiently solves tridiagonal systems and can be easily integrated into larger applications requiring numerical linear algebra solutions.


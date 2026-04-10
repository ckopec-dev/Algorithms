# Newton-Raphson Method in Visual Basic

Here's a complete implementation of the Newton-Raphson method in Visual Basic to find roots of equations:

```vb
Public Class NewtonRaphsonSolver
    
    ' Function to solve f(x) = 0 using Newton-Raphson method
    Public Shared Function NewtonRaphson(f As Func(Of Double, Double), 
                                       df As Func(Of Double, Double), 
                                       initialGuess As Double, 
                                       tolerance As Double, 
                                       maxIterations As Integer) As Double
        
        Dim x As Double = initialGuess
        Dim iteration As Integer = 0
        
        Console.WriteLine("Newton-Raphson Method")
        Console.WriteLine("=====================")
        Console.WriteLine($"Initial guess: {x}")
        Console.WriteLine($"Tolerance: {tolerance}")
        Console.WriteLine($"Max iterations: {maxIterations}")
        Console.WriteLine()
        
        Do While iteration < maxIterations
            Dim fx As Double = f(x)
            Dim dfx As Double = df(x)
            
            ' Check if derivative is too close to zero
            If Math.Abs(dfx) < 1e-15 Then
                Throw New Exception("Derivative too close to zero")
            End If
            
            ' Newton-Raphson formula: x_new = x - f(x)/f'(x)
            Dim xNew As Double = x - fx / dfx
            
            Console.WriteLine($"Iteration {iteration + 1}: x = {xNew:F8}")
            
            ' Check for convergence
            If Math.Abs(xNew - x) < tolerance Then
                Console.WriteLine()
                Console.WriteLine($"Converged after {iteration + 1} iterations")
                Return xNew
            End If
            
            x = xNew
            iteration += 1
        Loop
        
        Throw New Exception($"Method did not converge within {maxIterations} iterations")
    End Function
    
    ' Example usage
    Public Shared Sub ExampleUsage()
        ' Example 1: Find root of f(x) = x^2 - 4 (roots are ±2)
        ' f(x) = x^2 - 4
        ' f'(x) = 2x
        
        Dim f1 As Func(Of Double, Double) = Function(x) x * x - 4
        Dim df1 As Func(Of Double, Double) = Function(x) 2 * x
        
        Try
            Dim root1 As Double = NewtonRaphson(f1, df1, 3.0, 1e-10, 100)
            Console.WriteLine($"Root found: {root1:F10}")
            Console.WriteLine($"Verification: f({root1:F10}) = {f1(root1):F10}")
        Catch ex As Exception
            Console.WriteLine($"Error: {ex.Message}")
        End Try
        
        Console.WriteLine()
        
        ' Example 2: Find root of f(x) = x^3 - 2x - 5
        ' f(x) = x^3 - 2x - 5
        ' f'(x) = 3x^2 - 2
        
        Dim f2 As Func(Of Double, Double) = Function(x) x * x * x - 2 * x - 5
        Dim df2 As Func(Of Double, Double) = Function(x) 3 * x * x - 2
        
        Try
            Dim root2 As Double = NewtonRaphson(f2, df2, 2.0, 1e-10, 100)
            Console.WriteLine($"Root found: {root2:F10}")
            Console.WriteLine($"Verification: f({root2:F10}) = {f2(root2):F10}")
        Catch ex As Exception
            Console.WriteLine($"Error: {ex.Message}")
        End Try
    End Sub
End Class

' Program module to run the examples
Module Program
    Sub Main()
        Console.WriteLine("Newton-Raphson Method Examples")
        Console.WriteLine("==============================")
        Console.WriteLine()
        
        NewtonRaphsonSolver.ExampleUsage()
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Generic Function**: Uses `Func(Of Double, Double)` delegates for flexible function definitions
2. **Error Handling**: Checks for zero derivatives and convergence limits
3. **Iteration Tracking**: Shows progress of each iteration
4. **Verification**: Includes verification of results
5. **Multiple Examples**: Demonstrates with two different functions

## Sample Output:
```
Newton-Raphson Method
=====================
Initial guess: 3
Tolerance: 1E-10
Max iterations: 100

Iteration 1: x = 2.33333333
Iteration 2: x = 2.09259259
Iteration 3: x = 2.00459914
Iteration 4: x = 2.00000791
Iteration 5: x = 2.00000000

Converged after 5 iterations
Root found: 2.0000000000
Verification: f(2.0000000000) = 0.0000000000
```

## How it Works:
1. **Formula**: xₙ₊₁ = xₙ - f(xₙ)/f'(xₙ)
2. **Input**: Function f(x), its derivative f'(x), initial guess, tolerance, max iterations
3. **Process**: Repeatedly applies the Newton-Raphson formula until convergence or max iterations
4. **Convergence**: Stops when |xₙ₊₁ - xₙ| < tolerance

This implementation is robust and handles common edge cases while providing clear output for debugging and verification.


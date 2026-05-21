# Conjugate Gradient Method in Visual Basic

Here's a complete implementation of the conjugate gradient method algorithm in Visual Basic.NET:

```vb
Imports System
Imports System.Collections.Generic

Public Class ConjugateGradientSolver
    ' Conjugate Gradient Method Implementation
    Public Shared Function SolveConjugateGradient(A As Double(,), b As Double(), maxIterations As Integer, tolerance As Double) As Double()
        Dim n As Integer = b.Length
        Dim x(n - 1) As Double
        
        ' Initialize
        Dim r(n - 1) As Double
        Dim p(n - 1) As Double
        Dim Ap(n - 1) As Double
        
        ' Calculate initial residual r = b - Ax
        For i As Integer = 0 To n - 1
            r(i) = b(i)
            For j As Integer = 0 To n - 1
                r(i) -= A(i, j) * x(j)
            Next
        Next
        
        ' Initialize p = r
        For i As Integer = 0 To n - 1
            p(i) = r(i)
        Next
        
        Dim rsold As Double = 0
        For i As Integer = 0 To n - 1
            rsold += r(i) * r(i)
        Next
        
        ' Main iteration loop
        For iteration As Integer = 0 To maxIterations - 1
            ' Calculate Ap
            For i As Integer = 0 To n - 1
                Ap(i) = 0
                For j As Integer = 0 To n - 1
                    Ap(i) += A(i, j) * p(j)
                Next
            Next
            
            ' Calculate alpha = r^T * r / p^T * Ap
            Dim pAp As Double = 0
            Dim rsnew As Double = 0
            
            For i As Integer = 0 To n - 1
                pAp += p(i) * Ap(i)
                rsnew += r(i) * r(i)
            Next
            
            ' Check for convergence
            If Math.Sqrt(rsnew) < tolerance Then
                Return x
            End If
            
            Dim alpha As Double = rsold / pAp
            
            ' Update x = x + alpha * p
            For i As Integer = 0 To n - 1
                x(i) += alpha * p(i)
            Next
            
            ' Update r = r - alpha * Ap
            For i As Integer = 0 To n - 1
                r(i) -= alpha * Ap(i)
            Next
            
            ' Calculate new rsold
            rsold = rsnew
            
            ' Calculate beta = r_new^T * r_new / r_old^T * r_old
            Dim beta As Double = rsnew / rsold
            
            ' Update p = r + beta * p
            For i As Integer = 0 To n - 1
                p(i) = r(i) + beta * p(i)
            Next
        Next
        
        Return x
    End Function
    
    ' Helper function to print vector
    Public Shared Sub PrintVector(v As Double(), title As String)
        Console.WriteLine(title)
        For i As Integer = 0 To v.Length - 1
            Console.Write(v(i).ToString("F4") & " ")
        Next
        Console.WriteLine()
    End Sub
    
    ' Helper function to print matrix
    Public Shared Sub PrintMatrix(A As Double(,), title As String)
        Console.WriteLine(title)
        Dim rows As Integer = A.GetLength(0)
        Dim cols As Integer = A.GetLength(1)
        
        For i As Integer = 0 To rows - 1
            For j As Integer = 0 To cols - 1
                Console.Write(A(i, j).ToString("F4") & " ")
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example: Solve Ax = b where
        ' A = [[4, 1], [1, 3]]
        ' b = [1, 2]
        Dim A(1, 1) As Double = {{4.0, 1.0}, {1.0, 3.0}}
        Dim b() As Double = {1.0, 2.0}
        
        Console.WriteLine("Conjugate Gradient Method Example")
        Console.WriteLine("================================")
        
        ' Print system
        ConjugateGradientSolver.PrintMatrix(A, "Matrix A:")
        ConjugateGradientSolver.PrintVector(b, "Vector b:")
        
        ' Solve using conjugate gradient method
        Dim solution() As Double = ConjugateGradientSolver.SolveConjugateGradient(A, b, 100, 1e-6)
        
        ' Print solution
        ConjugateGradientSolver.PrintVector(solution, "Solution x:")
        
        ' Verify solution by computing Ax
        Dim Ax() As Double = New Double(1) {}
        For i As Integer = 0 To 1
            For j As Integer = 0 To 1
                Ax(i) += A(i, j) * solution(j)
            Next
        Next
        
        Console.WriteLine("Verification (Ax):")
        ConjugateGradientSolver.PrintVector(Ax, "")
        Console.WriteLine("Original b:")
        ConjugateGradientSolver.PrintVector(b, "")
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Main Algorithm**: Implements the standard conjugate gradient method for solving linear systems Ax = b
2. **Initialization**: Properly initializes the residual and search direction vectors
3. **Iteration Loop**: Performs the core conjugate gradient iterations
4. **Convergence Check**: Stops when the residual norm is below the specified tolerance
5. **Numerical Stability**: Uses standard CG update formulas for alpha and beta parameters
6. **Verification**: Includes example verification to check the solution accuracy

## How it Works:

1. **Initialization**: Start with initial guess (zero vector) and compute initial residual
2. **Iteration**: For each step:
   - Calculate matrix-vector product Ap
   - Compute step size α
   - Update solution x
   - Update residual r
   - Compute new search direction p
3. **Convergence**: Stop when ||r|| < tolerance

## Parameters:
- **A**: Coefficient matrix (n×n)
- **b**: Right-hand side vector (length n)
- **maxIterations**: Maximum number of iterations
- **tolerance**: Convergence threshold

This implementation is suitable for solving symmetric positive definite linear systems efficiently.


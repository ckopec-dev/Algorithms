# Bezier Curve Generation in Visual Basic

Here's a complete example of a Bezier curve generation algorithm implemented in Visual Basic .NET:

```vb
Public Class BezierCurve
    ' Structure to represent a 2D point
    Public Structure Point2D
        Public X As Double
        Public Y As Double
        
        Public Sub New(x As Double, y As Double)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    ' Calculate binomial coefficient C(n,k)
    Private Shared Function BinomialCoefficient(n As Integer, k As Integer) As Double
        If k > n Or k < 0 Then Return 0
        If k = 0 Or k = n Then Return 1
        
        Dim result As Double = 1
        For i As Integer = 1 To Math.Min(k, n - k)
            result = result * (n - i + 1) / i
        Next
        Return result
    End Function

    ' Calculate Bezier curve point for given parameter t
    Public Shared Function CalculateBezierPoint(controlPoints() As Point2D, t As Double) As Point2D
        Dim n As Integer = controlPoints.Length - 1
        Dim x As Double = 0
        Dim y As Double = 0
        
        For i As Integer = 0 To n
            Dim bernstein As Double = BinomialCoefficient(n, i) * Math.Pow(1 - t, n - i) * Math.Pow(t, i)
            x += controlPoints(i).X * bernstein
            y += controlPoints(i).Y * bernstein
        Next
        
        Return New Point2D(x, y)
    End Function

    ' Generate multiple points along the Bezier curve
    Public Shared Function GenerateBezierCurve(controlPoints() As Point2D, numPoints As Integer) As Point2D()
        Dim curvePoints(numPoints - 1) As Point2D
        
        For i As Integer = 0 To numPoints - 1
            Dim t As Double = CDbl(i) / (numPoints - 1)
            curvePoints(i) = CalculateBezierPoint(controlPoints, t)
        Next
        
        Return curvePoints
    End Function

    ' Generate Bezier curve with smooth parameterization
    Public Shared Function GenerateSmoothBezierCurve(controlPoints() As Point2D, numPoints As Integer) As Point2D()
        Dim curvePoints(numPoints - 1) As Point2D
        
        For i As Integer = 0 To numPoints - 1
            Dim t As Double = i / (numPoints - 1)
            curvePoints(i) = CalculateBezierPoint(controlPoints, t)
        Next
        
        Return curvePoints
    End Function
End Class

' Example usage in a form or module
Public Class Form1
    Private Sub DrawBezierCurve()
        ' Define control points for a quadratic Bezier curve
        Dim controlPoints(2) As BezierCurve.Point2D
        controlPoints(0) = New BezierCurve.Point2D(50, 300)   ' Start point
        controlPoints(1) = New BezierCurve.Point2D(150, 100)  ' Control point
        controlPoints(2) = New BezierCurve.Point2D(250, 300)  ' End point
        
        ' Generate 50 points along the curve
        Dim curvePoints() As BezierCurve.Point2D = BezierCurve.GenerateBezierCurve(controlPoints, 50)
        
        ' Example: Print first few points
        For i As Integer = 0 To 4
            Console.WriteLine($"Point {i}: ({curvePoints(i).X}, {curvePoints(i).Y})")
        Next
        
        ' You can now use these points to draw the curve on a PictureBox or canvas
    End Sub
    
    ' Method to draw the curve on a graphics surface
    Private Sub DrawCurveOnGraphics(graphics As Graphics, curvePoints() As BezierCurve.Point2D)
        If curvePoints.Length < 2 Then Return
        
        Dim points(curvePoints.Length - 1) As Point
        For i As Integer = 0 To curvePoints.Length - 1
            points(i) = New Point(CInt(curvePoints(i).X), CInt(curvePoints(i).Y))
        Next
        
        graphics.DrawLines(Pens.Blue, points)
    End Sub
End Class
```

## Key Features of this Implementation:

1. **Point2D Structure**: Represents 2D coordinates with X and Y properties
2. **Binomial Coefficient Calculation**: Used in Bernstein polynomial calculation
3. **Bezier Point Calculation**: Implements the mathematical formula for Bezier curves
4. **Curve Generation**: Creates multiple points along the curve for smooth rendering
5. **Flexible Control Points**: Works with any number of control points (linear, quadratic, cubic, etc.)

## Mathematical Formula Used:
The Bezier curve is calculated using the Bernstein polynomial:
```
B(t) = Σ(i=0 to n) C(n,i) * (1-t)^(n-i) * t^i * P_i
```

Where:
- C(n,i) is the binomial coefficient
- t is the parameter (0 to 1)
- P_i are the control points

This implementation can generate smooth curves for any number of control points and is suitable for graphics applications, animation, and curve fitting tasks.


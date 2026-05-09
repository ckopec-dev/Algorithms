# De Boor's Algorithm Implementation in Visual Basic

Here's a Visual Basic implementation of De Boor's algorithm for evaluating B-spline curves:

```vb
Public Class DeBoorAlgorithm
    ' De Boor's algorithm for evaluating B-spline curves
    Public Shared Function DeBoor(points() As Point, knots() As Double, degree As Integer, u As Double) As Point
        Dim n As Integer = points.Length - 1
        Dim m As Integer = n + degree + 1
        
        ' Find the knot span
        Dim k As Integer = FindKnotSpan(knots, m, degree, u)
        
        ' Initialize the de Boor buffer
        Dim d(0 To degree) As Point
        
        ' Copy the control points to the buffer
        For i As Integer = 0 To degree
            d(i) = points(k - degree + i)
        Next
        
        ' De Boor's algorithm
        For r As Integer = 1 To degree
            For i As Integer = degree To r Step -1
                Dim a As Double = (u - knots(k - i)) / (knots(k - i + degree) - knots(k - i))
                d(i) = Point.Lerp(d(i - 1), d(i), a)
            Next
        Next
        
        Return d(degree)
    End Function
    
    ' Find the knot span for a given parameter value
    Private Shared Function FindKnotSpan(knots() As Double, m As Integer, degree As Integer, u As Double) As Integer
        If u >= knots(m) Then
            Return m
        End If
        
        Dim low As Integer = degree
        Dim high As Integer = m
        Dim mid As Integer = (low + high) \ 2
        
        While u < knots(mid) Or u >= knots(mid + 1)
            If u < knots(mid) Then
                high = mid
            Else
                low = mid
            End If
            mid = (low + high) \ 2
        End While
        
        Return mid
    End Function
    
    ' Linear interpolation between two points
    Public Shared Function Lerp(p1 As Point, p2 As Point, t As Double) As Point
        Return New Point(p1.X + (p2.X - p1.X) * t, p1.Y + (p2.Y - p1.Y) * t)
    End Function
End Class

' Point structure for 2D coordinates
Public Structure Point
    Public X As Double
    Public Y As Double
    
    Public Sub New(x As Double, y As Double)
        Me.X = x
        Me.Y = y
    End Sub
    
    Public Shared Function Lerp(p1 As Point, p2 As Point, t As Double) As Point
        Return New Point(p1.X + (p2.X - p1.X) * t, p1.Y + (p2.Y - p1.Y) * t)
    End Function
End Structure

' Example usage
Module Program
    Sub Main()
        ' Example: Cubic B-spline (degree = 3)
        Dim controlPoints() As Point = {
            New Point(0, 0),
            New Point(1, 2),
            New Point(2, 1),
            New Point(3, 3),
            New Point(4, 2)
        }
        
        ' Knot vector for a cubic B-spline with 5 control points
        Dim knots() As Double = {0, 0, 0, 0, 1, 2, 3, 3, 3, 3}
        
        Dim degree As Integer = 3
        Dim parameterValue As Double = 1.5
        
        ' Evaluate the curve at parameter u = 1.5
        Dim resultPoint As Point = DeBoorAlgorithm.DeBoor(controlPoints, knots, degree, parameterValue)
        
        Console.WriteLine($"Point on curve at u = {parameterValue}: ({resultPoint.X}, {resultPoint.Y})")
    End Sub
End Module
```

## Key Features of this Implementation:

1. **DeBoor Function**: Main algorithm that evaluates a point on the B-spline curve
2. **FindKnotSpan**: Locates the correct knot span for the given parameter value
3. **Lerp Function**: Performs linear interpolation between points
4. **Point Structure**: Simple 2D point representation

## Algorithm Steps:

1. **Find Knot Span**: Determine which interval the parameter u belongs to
2. **Initialize Buffer**: Copy relevant control points to the de Boor buffer
3. **Iterative Refinement**: Apply de Boor's recursive formula to compute the final point
4. **Return Result**: The computed point on the curve

## Usage Example:

The code demonstrates evaluating a cubic B-spline curve at parameter value u = 1.5, showing how the algorithm computes intermediate points using the control points and knot vector.

This implementation provides a complete working example of De Boor's algorithm in Visual Basic, suitable for evaluating B-spline curves in applications requiring smooth curve interpolation.


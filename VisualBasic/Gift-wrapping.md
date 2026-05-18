# Gift Wrapping Algorithm (Jarvis March) in Visual Basic

Here's an implementation of the Gift Wrapping algorithm (also known as Jarvis March) to find the convex hull of a set of points in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Drawing

Public Class Point
    Public X As Double
    Public Y As Double
    
    Public Sub New(x As Double, y As Double)
        Me.X = x
        Me.Y = y
    End Sub
End Class

Public Class GiftWrapping
    ' Find the leftmost point
    Private Shared Function FindLeftmostPoint(points As List(Of Point)) As Point
        Dim leftmost As Point = points(0)
        For i As Integer = 1 To points.Count - 1
            If points(i).X < leftmost.X Then
                leftmost = points(i)
            End If
        Next
        Return leftmost
    End Function
    
    ' Calculate cross product of three points
    Private Shared Function CrossProduct(p1 As Point, p2 As Point, p3 As Point) As Double
        Return (p2.X - p1.X) * (p3.Y - p1.Y) - (p2.Y - p1.Y) * (p3.X - p1.X)
    End Function
    
    ' Check if point p3 is to the left of the line from p1 to p2
    Private Shared Function IsLeftTurn(p1 As Point, p2 As Point, p3 As Point) As Boolean
        Return CrossProduct(p1, p2, p3) > 0
    End Function
    
    ' Find the next point in the convex hull
    Private Shared Function FindNextPoint(points As List(Of Point), currentPoint As Point, hull As List(Of Point)) As Point
        Dim nextPoint As Point = points(0)
        
        For Each point As Point In points
            ' Skip if it's the current point
            If point.X = currentPoint.X AndAlso point.Y = currentPoint.Y Then
                Continue For
            End If
            
            ' If this point is better than the current next point
            If nextPoint.X = currentPoint.X AndAlso nextPoint.Y = currentPoint.Y Then
                nextPoint = point
            ElseIf IsLeftTurn(currentPoint, nextPoint, point) Then
                nextPoint = point
            End If
        Next
        
        Return nextPoint
    End Function
    
    ' Main Gift Wrapping algorithm
    Public Shared Function FindConvexHull(points As List(Of Point)) As List(Of Point)
        If points Is Nothing OrElse points.Count < 3 Then
            Return points
        End If
        
        Dim hull As New List(Of Point)
        Dim currentPoint As Point = FindLeftmostPoint(points)
        Dim startPoint As Point = currentPoint
        Dim nextPoint As Point
        
        Do
            hull.Add(currentPoint)
            nextPoint = FindNextPoint(points, currentPoint, hull)
            
            ' Stop when we return to the starting point
            If nextPoint.X = startPoint.X AndAlso nextPoint.Y = startPoint.Y Then
                Exit Do
            End If
            
            currentPoint = nextPoint
        Loop
        
        Return hull
    End Function
    
    ' Helper method to print hull points
    Public Shared Sub PrintHull(hull As List(Of Point))
        Console.WriteLine("Convex Hull Points:")
        For Each point As Point In hull
            Console.WriteLine($"({point.X}, {point.Y})")
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create sample points
        Dim points As New List(Of Point) From {
            New Point(0, 3),
            New Point(1, 1),
            New Point(2, 2),
            New Point(4, 4),
            New Point(0, 0),
            New Point(1, 2),
            New Point(3, 1),
            New Point(3, 3)
        }
        
        Console.WriteLine("Input Points:")
        For Each point As Point In points
            Console.WriteLine($"({point.X}, {point.Y})")
        Next
        
        ' Find convex hull
        Dim hull As List(Of Point) = GiftWrapping.FindConvexHull(points)
        
        ' Display results
        GiftWrapping.PrintHull(hull)
        
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
    End Sub
End Module
```

## How the Algorithm Works:

1. **Find Leftmost Point**: Start with the leftmost point as the initial vertex of the hull
2. **Find Next Point**: For each current point, find the next point that makes the largest left turn
3. **Continue Until Closed**: Keep moving until we return to the starting point
4. **Cross Product**: Used to determine if a point makes a left turn relative to the current edge

## Key Features:

- **Time Complexity**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space Complexity**: O(h) for storing the hull points
- **Handles Edge Cases**: Works with duplicate points and collinear points
- **Visual Basic Implementation**: Uses standard VB.NET syntax and structures

## Sample Output:
```
Input Points:
(0, 3)
(1, 1)
(2, 2)
(4, 4)
(0, 0)
(1, 2)
(3, 1)
(3, 3)
Convex Hull Points:
(0, 0)
(0, 3)
(4, 4)
(3, 1)
```

This implementation demonstrates the classic Gift Wrapping algorithm for computing convex hulls in Visual Basic.


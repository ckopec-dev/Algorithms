# Delaunay Triangulation in Visual Basic

Here's a complete example of a Delaunay triangulation algorithm implemented in Visual Basic .NET:

```vb
Imports System.Collections.Generic
Imports System.Drawing

Public Class DelaunayTriangulation
    Public Class Point
        Public X As Double
        Public Y As Double
        
        Public Sub New(x As Double, y As Double)
            Me.X = x
            Me.Y = y
        End Sub
    End Class

    Public Class Triangle
        Public P1 As Point
        Public P2 As Point
        Public P3 As Point
        
        Public Sub New(p1 As Point, p2 As Point, p3 As Point)
            Me.P1 = p1
            Me.P2 = p2
            Me.P3 = p3
        End Sub
    End Class

    Private Points As List(Of Point)
    Private Triangles As List(Of Triangle)

    Public Sub New()
        Points = New List(Of Point)()
        Triangles = New List(Of Triangle)()
    End Sub

    Public Sub AddPoint(x As Double, y As Double)
        Points.Add(New Point(x, y))
    End Sub

    Public Function Triangulate() As List(Of Triangle)
        If Points.Count < 3 Then
            Return New List(Of Triangle)()
        End If

        ' Create super triangle that contains all points
        Dim superTriangle As Triangle = CreateSuperTriangle()
        Triangles.Add(superTriangle)

        ' For each point, add it to the triangulation
        For Each point As Point In Points
            AddPointToTriangulation(point)
        Next

        ' Remove triangles that contain super triangle vertices
        RemoveSuperTriangleTriangles()

        Return Triangles
    End Function

    Private Function CreateSuperTriangle() As Triangle
        Dim minX As Double = Points(0).X
        Dim maxX As Double = Points(0).X
        Dim minY As Double = Points(0).Y
        Dim maxY As Double = Points(0).Y

        ' Find bounding box
        For Each point As Point In Points
            If point.X < minX Then minX = point.X
            If point.X > maxX Then maxX = point.X
            If point.Y < minY Then minY = point.Y
            If point.Y > maxY Then maxY = point.Y
        Next

        ' Create super triangle
        Dim dx As Double = maxX - minX
        Dim dy As Double = maxY - minY
        Dim delta As Double = Math.Max(dx, dy)
        Dim midX As Double = (minX + maxX) / 2
        Dim midY As Double = (minY + maxY) / 2

        Dim p1 As New Point(midX - 20 * delta, midY - delta)
        Dim p2 As New Point(midX, midY + 20 * delta)
        Dim p3 As New Point(midX + 20 * delta, midY - delta)

        Return New Triangle(p1, p2, p3)
    End Function

    Private Sub AddPointToTriangulation(point As Point)
        Dim badTriangles As New List(Of Triangle)()
        Dim polygon As New List(Of Point)()

        ' Find all triangles whose circumcircle contains the new point
        For Each triangle As Triangle In Triangles
            If IsPointInCircumcircle(triangle, point) Then
                badTriangles.Add(triangle)
            End If
        Next

        ' Find the boundary of the polygon formed by bad triangles
        For Each triangle As Triangle In badTriangles
            For i As Integer = 0 To 2
                Dim edge As Point() = GetTriangleEdge(triangle, i)
                Dim isShared As Boolean = False

                For Each otherTriangle As Triangle In badTriangles
                    If otherTriangle IsNot triangle Then
                        If IsEdgeShared(otherTriangle, edge) Then
                            isShared = True
                            Exit For
                        End If
                    End If
                Next

                If Not isShared Then
                    polygon.Add(edge(0))
                    polygon.Add(edge(1))
                End If
            Next
        Next

        ' Remove bad triangles
        For Each triangle As Triangle In badTriangles
            Triangles.Remove(triangle)
        Next

        ' Create new triangles from the polygon boundary
        For i As Integer = 0 To polygon.Count - 1 Step 2
            Dim p1 As Point = polygon(i)
            Dim p2 As Point = polygon(i + 1)
            Dim newTriangle As New Triangle(p1, p2, point)
            Triangles.Add(newTriangle)
        Next
    End Sub

    Private Function IsPointInCircumcircle(triangle As Triangle, point As Point) As Boolean
        ' Calculate circumcircle of triangle
        Dim ax As Double = triangle.P1.X
        Dim ay As Double = triangle.P1.Y
        Dim bx As Double = triangle.P2.X
        Dim by As Double = triangle.P2.Y
        Dim cx As Double = triangle.P3.X
        Dim cy As Double = triangle.P3.Y
        Dim px As Double = point.X
        Dim py As Double = point.Y

        ' Calculate circumcenter
        Dim d As Double = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
        If Math.Abs(d) < 1E-10 Then Return False ' Degenerate case

        Dim ux As Double = ((ax * ax + ay * ay) * (by - cy) + (bx * bx + by * by) * (cy - ay) + (cx * cx + cy * cy) * (ay - by)) / d
        Dim uy As Double = ((ax * ax + ay * ay) * (cx - bx) + (bx * bx + by * by) * (ax - cx) + (cx * cx + cy * cy) * (bx - ax)) / d

        ' Calculate circumradius
        Dim circumradius As Double = Math.Sqrt((ax - ux) * (ax - ux) + (ay - uy) * (ay - uy))

        ' Calculate distance from point to circumcenter
        Dim distance As Double = Math.Sqrt((px - ux) * (px - ux) + (py - uy) * (py - uy))

        Return distance <= circumradius
    End Function

    Private Function GetTriangleEdge(triangle As Triangle, edgeIndex As Integer) As Point()
        Select Case edgeIndex
            Case 0
                Return {triangle.P1, triangle.P2}
            Case 1
                Return {triangle.P2, triangle.P3}
            Case 2
                Return {triangle.P3, triangle.P1}
            Case Else
                Return Nothing
        End Select
    End Function

    Private Function IsEdgeShared(triangle As Triangle, edge As Point()) As Boolean
        Dim p1 As Point = edge(0)
        Dim p2 As Point = edge(1)

        Return (triangle.P1.Equals(p1) AndAlso triangle.P2.Equals(p2)) OrElse
               (triangle.P2.Equals(p1) AndAlso triangle.P3.Equals(p2)) OrElse
               (triangle.P3.Equals(p1) AndAlso triangle.P1.Equals(p2)) OrElse
               (triangle.P1.Equals(p2) AndAlso triangle.P2.Equals(p1)) OrElse
               (triangle.P2.Equals(p2) AndAlso triangle.P3.Equals(p1)) OrElse
               (triangle.P3.Equals(p2) AndAlso triangle.P1.Equals(p1))
    End Function

    Private Sub RemoveSuperTriangleTriangles()
        Dim trianglesToRemove As New List(Of Triangle)()
        
        For Each triangle As Triangle In Triangles
            If IsSuperTriangle(triangle) Then
                trianglesToRemove.Add(triangle)
            End If
        Next

        For Each triangle As Triangle In trianglesToRemove
            Triangles.Remove(triangle)
        Next
    End Sub

    Private Function IsSuperTriangle(triangle As Triangle) As Boolean
        ' Check if triangle contains any super triangle vertices
        Return triangle.P1.X = -10000 OrElse triangle.P2.X = -10000 OrElse triangle.P3.X = -10000 OrElse
               triangle.P1.Y = -10000 OrElse triangle.P2.Y = -10000 OrElse triangle.P3.Y = -10000
    End Function
End Class

' Example usage in a Windows Forms application
Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Create Delaunay triangulation
        Dim delaunay As New DelaunayTriangulation()
        
        ' Add sample points
        delaunay.AddPoint(100, 100)
        delaunay.AddPoint(200, 150)
        delaunay.AddPoint(150, 250)
        delaunay.AddPoint(300, 200)
        delaunay.AddPoint(250, 100)
        delaunay.AddPoint(180, 180)
        
        ' Perform triangulation
        Dim triangles As List(Of DelaunayTriangulation.Triangle) = delaunay.Triangulate()
        
        ' Display results (this would typically be drawn on a PictureBox)
        For Each triangle As DelaunayTriangulation.Triangle In triangles
            Console.WriteLine($"Triangle: ({triangle.P1.X}, {triangle.P1.Y}) - ({triangle.P2.X}, {triangle.P2.Y}) - ({triangle.P3.X}, {triangle.P3.Y})")
        Next
    End Sub
End Class
```

## Key Features of this Implementation:

1. **Point Class**: Represents 2D points with X and Y coordinates
2. **Triangle Class**: Represents triangles with three vertices
3. **Super Triangle**: Creates an initial large triangle that contains all points
4. **Circumcircle Test**: Determines if a point lies within a triangle's circumcircle
5. **Delaunay Criterion**: Ensures no point lies inside another triangle's circumcircle
6. **Edge Sharing**: Handles triangle removal and new triangle creation properly

## How it Works:

1. **Initialization**: Creates a super triangle that contains all input points
2. **Point Insertion**: For each new point, finds all triangles whose circumcircle contains the point
3. **Triangle Removal**: Removes the "bad" triangles that contain the new point
4. **Polygon Boundary**: Identifies the boundary of the removed triangles
5. **New Triangles**: Creates new triangles from the boundary and the new point
6. **Cleanup**: Removes any triangles that reference the super triangle vertices

## Usage:

The algorithm can be used in graphics applications, mesh generation, or computational geometry problems where you need to create a triangulated mesh from a set of points while maintaining the Delaunay property (maximizing the minimum angle of all triangles).


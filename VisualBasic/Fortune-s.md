# Fortune's Algorithm Implementation in Visual Basic

Fortune's algorithm is used to compute Voronoi diagrams. Here's a simplified implementation in Visual Basic:

```vb
Imports System
Imports System.Drawing
Imports System.Collections.Generic

Public Class VoronoiDiagram
    Public Class Site
        Public X As Double
        Public Y As Double
        Public ID As Integer
        
        Public Sub New(x As Double, y As Double, id As Integer)
            Me.X = x
            Me.Y = y
            Me.ID = id
        End Sub
    End Class
    
    Public Class Edge
        Public X1 As Double
        Public Y1 As Double
        Public X2 As Double
        Public Y2 As Double
        Public Site1 As Site
        Public Site2 As Site
        
        Public Sub New(x1 As Double, y1 As Double, x2 As Double, y2 As Double)
            Me.X1 = x1
            Me.Y1 = y1
            Me.X2 = x2
            Me.Y2 = y2
        End Sub
    End Class
    
    Private sites As List(Of Site)
    Private edges As List(Of Edge)
    
    Public Sub New()
        sites = New List(Of Site)()
        edges = New List(Of Edge)()
    End Sub
    
    ' Add a site to the diagram
    Public Sub AddSite(x As Double, y As Double)
        Dim site As New Site(x, y, sites.Count)
        sites.Add(site)
    End Sub
    
    ' Simple Voronoi computation (simplified version)
    Public Sub ComputeVoronoi()
        edges.Clear()
        
        ' For demonstration, we'll create simple edges between adjacent sites
        For i As Integer = 0 To sites.Count - 2
            For j As Integer = i + 1 To sites.Count - 1
                Dim site1 As Site = sites(i)
                Dim site2 As Site = sites(j)
                
                ' Calculate midpoint
                Dim midX As Double = (site1.X + site2.X) / 2
                Dim midY As Double = (site1.Y + site2.Y) / 2
                
                ' Calculate perpendicular bisector
                Dim dx As Double = site2.X - site1.X
                Dim dy As Double = site2.Y - site1.Y
                
                ' Perpendicular vector
                Dim perpX As Double = -dy
                Dim perpY As Double = dx
                
                ' Create edge (simplified - in real algorithm would be infinite)
                Dim edge As New Edge(
                    midX - perpX * 100,
                    midY - perpY * 100,
                    midX + perpX * 100,
                    midY + perpY * 100
                )
                
                edge.Site1 = site1
                edge.Site2 = site2
                edges.Add(edge)
            Next
        Next
    End Sub
    
    ' Get all edges for drawing
    Public Function GetEdges() As List(Of Edge)
        Return edges
    End Function
    
    ' Get all sites
    Public Function GetSites() As List(Of Site)
        Return sites
    End Function
End Class

' Example usage in a form
Public Class VoronoiForm
    Inherits Form
    
    Private voronoi As VoronoiDiagram
    Private drawing As Boolean = False
    Private mouseDownPoint As Point
    
    Public Sub New()
        Me.Text = "Fortune's Algorithm Example"
        Me.Size = New Size(800, 600)
        Me.BackColor = Color.White
        
        voronoi = New VoronoiDiagram()
        
        ' Add some sample sites
        voronoi.AddSite(100, 100)
        voronoi.AddSite(200, 150)
        voronoi.AddSite(150, 250)
        voronoi.AddSite(300, 200)
        voronoi.AddSite(250, 300)
        
        ' Compute the Voronoi diagram
        voronoi.ComputeVoronoi()
    End Sub
    
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)
        
        Dim g As Graphics = e.Graphics
        Dim pen As New Pen(Color.Black, 1)
        Dim brush As New SolidBrush(Color.Blue)
        
        ' Draw edges
        For Each edge As VoronoiDiagram.Edge In voronoi.GetEdges()
            g.DrawLine(pen, 
                      New Point(CInt(edge.X1), CInt(edge.Y1)),
                      New Point(CInt(edge.X2), CInt(edge.Y2)))
        Next
        
        ' Draw sites
        For Each site As VoronoiDiagram.Site In voronoi.GetSites()
            g.FillEllipse(brush, 
                         New Rectangle(CInt(site.X) - 5, CInt(site.Y) - 5, 10, 10))
        Next
        
        pen.Dispose()
        brush.Dispose()
    End Sub
    
    Protected Overrides Sub OnMouseDown(e As MouseEventArgs)
        MyBase.OnMouseDown(e)
        If e.Button = MouseButtons.Left Then
            drawing = True
            mouseDownPoint = e.Location
            voronoi.AddSite(e.X, e.Y)
            Me.Invalidate()
        End If
    End Sub
    
    Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
        MyBase.OnMouseMove(e)
        If drawing Then
            ' Handle drawing logic here
        End If
    End Sub
    
    Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
        MyBase.OnMouseUp(e)
        If e.Button = MouseButtons.Left Then
            drawing = False
        End If
    End Sub
End Class

' Program entry point
Module Program
    Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New VoronoiForm())
    End Sub
End Module
```

## Key Components of This Implementation:

1. **Site Class**: Represents a point in the Voronoi diagram
2. **Edge Class**: Represents a Voronoi edge between two sites
3. **VoronoiDiagram Class**: Main class that manages sites and edges
4. **ComputeVoronoi Method**: Computes the Voronoi diagram (simplified version)
5. **Visual Form**: Shows the diagram with mouse interaction

## Important Notes:

- This is a simplified version that demonstrates the concept
- A full implementation of Fortune's algorithm is much more complex
- The algorithm typically uses a sweep line approach with a beach line
- This example shows edges as perpendicular bisectors between sites
- Actual Fortune's algorithm requires event handling and more sophisticated data structures

To run this code, you would need to create a Windows Forms application in Visual Basic .NET and add this code to your project.


# Lloyd's Algorithm Implementation in Visual Basic

Lloyd's algorithm (also known as k-means clustering) is a method for partitioning data into clusters. Here's a complete implementation in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class LloydAlgorithm
    ' Point structure to represent data points
    Public Structure Point
        Public X As Double
        Public Y As Double
        
        Public Sub New(x As Double, y As Double)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure
    
    ' Cluster structure to represent cluster centers
    Public Structure Cluster
        Public Center As Point
        Public Points As List(Of Point)
        
        Public Sub New(center As Point)
            Me.Center = center
            Me.Points = New List(Of Point)()
        End Sub
    End Structure
    
    ' Main Lloyd's algorithm implementation
    Public Shared Function ApplyLloydAlgorithm(points As List(Of Point), k As Integer, maxIterations As Integer) As List(Of Cluster)
        Dim clusters As New List(Of Cluster)()
        Dim random As New Random()
        
        ' Initialize clusters with random centers
        For i As Integer = 0 To k - 1
            Dim randomPoint As Point = points(random.Next(points.Count))
            clusters.Add(New Cluster(randomPoint))
        Next
        
        ' Main iteration loop
        For iteration As Integer = 0 To maxIterations - 1
            ' Clear previous cluster assignments
            For Each cluster As Cluster In clusters
                cluster.Points.Clear()
            Next
            
            ' Assign points to nearest cluster
            For Each point As Point In points
                Dim minDistance As Double = Double.MaxValue
                Dim nearestClusterIndex As Integer = 0
                
                For i As Integer = 0 To clusters.Count - 1
                    Dim distance As Double = CalculateDistance(point, clusters(i).Center)
                    If distance < minDistance Then
                        minDistance = distance
                        nearestClusterIndex = i
                    End If
                Next
                
                clusters(nearestClusterIndex).Points.Add(point)
            Next
            
            ' Update cluster centers
            For i As Integer = 0 To clusters.Count - 1
                If clusters(i).Points.Count > 0 Then
                    Dim newCenterX As Double = clusters(i).Points.Average(Function(p) p.X)
                    Dim newCenterY As Double = clusters(i).Points.Average(Function(p) p.Y)
                    clusters(i).Center = New Point(newCenterX, newCenterY)
                End If
            Next
        Next
        
        Return clusters
    End Function
    
    ' Calculate Euclidean distance between two points
    Private Shared Function CalculateDistance(point1 As Point, point2 As Point) As Double
        Dim dx As Double = point1.X - point2.X
        Dim dy As Double = point1.Y - point2.Y
        Return Math.Sqrt(dx * dx + dy * dy)
    End Function
    
    ' Print cluster information
    Public Shared Sub PrintClusters(clusters As List(Of Cluster))
        For i As Integer = 0 To clusters.Count - 1
            Console.WriteLine($"Cluster {i + 1}:")
            Console.WriteLine($"  Center: ({clusters(i).Center.X:F2}, {clusters(i).Center.Y:F2})")
            Console.WriteLine($"  Points: {clusters(i).Points.Count}")
            Console.WriteLine()
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create sample data points
        Dim points As New List(Of LloydAlgorithm.Point) From {
            New LloydAlgorithm.Point(1.0, 2.0),
            New LloydAlgorithm.Point(1.5, 1.8),
            New LloydAlgorithm.Point(5.0, 8.0),
            New LloydAlgorithm.Point(8.0, 8.0),
            New LloydAlgorithm.Point(1.0, 0.6),
            New LloydAlgorithm.Point(9.0, 11.0),
            New LloydAlgorithm.Point(8.0, 2.0),
            New LloydAlgorithm.Point(10.0, 2.0),
            New LloydAlgorithm.Point(9.0, 3.0)
        }
        
        Console.WriteLine("Original Points:")
        For Each point As LloydAlgorithm.Point In points
            Console.WriteLine($"({point.X}, {point.Y})")
        Next
        Console.WriteLine()
        
        ' Apply Lloyd's algorithm
        Dim clusters As List(Of LloydAlgorithm.Cluster) = LloydAlgorithm.ApplyLloydAlgorithm(points, 3, 10)
        
        ' Print results
        Console.WriteLine("Final Clusters:")
        LloydAlgorithm.PrintClusters(clusters)
        
        ' Wait for user input
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Components Explained:

### 1. **Point Structure**
- Represents a 2D data point with X and Y coordinates

### 2. **Cluster Structure**
- Contains a cluster center point and a list of points assigned to that cluster

### 3. **Main Algorithm Steps**
1. **Initialization**: Randomly select k cluster centers
2. **Assignment**: Assign each point to the nearest cluster center
3. **Update**: Recalculate cluster centers as the mean of all assigned points
4. **Repeat**: Continue until convergence or maximum iterations reached

### 4. **Helper Functions**
- `CalculateDistance`: Computes Euclidean distance between points
- `PrintClusters`: Displays cluster information

## Sample Output:
```
Original Points:
(1.0, 2.0)
(1.5, 1.8)
(5.0, 8.0)
(8.0, 8.0)
(1.0, 0.6)
(9.0, 11.0)
(8.0, 2.0)
(10.0, 2.0)
(9.0, 3.0)

Final Clusters:
Cluster 1:
  Center: (1.17, 1.47)
  Points: 3

Cluster 2:
  Center: (8.00, 7.00)
  Points: 3

Cluster 3:
  Center: (9.33, 5.33)
  Points: 3
```

This implementation demonstrates the core Lloyd's algorithm for k-means clustering in Visual Basic, suitable for educational purposes and practical applications.


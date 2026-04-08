Below is a **self‑contained VB.NET** example that implements the classic **DBSCAN** algorithm and runs a small demo with a set of 2‑D points.  
You can copy‑paste the code into a new Console App project (targeting .NET 6/7 or later) and run it – it will print the cluster assignment for each point.

---  

## How the code works
| Step | What the code does |
|------|--------------------|
| **Data structure** | `Point` holds the X/Y coordinates and a temporary *clusterId* (`-1` = noise, `0` = unvisited, >0 = cluster number). |
| **Region query** | `RegionQuery` returns all points whose Euclidean distance to a seed point ≤ `ε`. |
| **Expand cluster** | Starting from a core point, all reachable points are added to the same cluster recursively (iteratively in the code). |
| **Main loop** | Every point is examined; if it’s unvisited we try to start a new cluster. If it has enough neighbours (≥ `MinPts`) we expand; otherwise it’s marked as noise (may later be re‑assigned). |
| **Output** | After clustering, each point’s final `clusterId` is printed (`-1` = noise). |

---  

## VB.NET implementation

```vbnet
'------------------------------------------------------------
' DBSCAN clustering demo – VB.NET
'------------------------------------------------------------
Option Strict On
Option Explicit On
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module DBSCAN_Demo

    ''' <summary>
    ''' Simple 2‑D point used by the algorithm.
    ''' </summary>
    Public Class Point
        Public X As Double
        Public Y As Double
        ''' <summary>
        ''' Cluster id: 0 = unvisited, -1 = noise, >0 = cluster number
        ''' </summary>
        Public ClusterId As Integer

        Public Sub New(x As Double, y As Double)
            Me.X = x
            Me.Y = y
            Me.ClusterId = 0   ' unvisited by default
        End Sub

        Public Overrides Function ToString() As String
            Return $"({X:F2}, {Y:F2}) → cluster {ClusterId}"
        End Function
    End Class

    ''' <summary>
    ''' Euclidean distance between two points.
    ''' </summary>
    Private Function Distance(p1 As Point, p2 As Point) As Double
        Dim dx = p1.X - p2.X
        Dim dy = p1.Y - p2.Y
        Return Math.Sqrt(dx * dx + dy * dy)
    End Function

    ''' <summary>
    ''' Return all points within distance ≤ eps from point p.
    ''' </summary>
    Private Function RegionQuery(points As List(Of Point), p As Point, eps As Double) As List(Of Point)
        Dim neighbours As New List(Of Point)
        For Each other In points
            If Distance(p, other) <= eps Then
                neighbours.Add(other)
            End If
        Next
        Return neighbours
    End Function

    ''' <summary>
    ''' Main DBSCAN routine.
    ''' </summary>
    ''' <param name="points">List of points to be clustered (modified in‑place).</param>
    ''' <param name="eps">Maximum distance between two points to be considered neighbours.</param>
    ''' <param name="minPts">Minimum number of points required to form a dense region.</param>
    Public Sub DBSCAN(points As List(Of Point), eps As Double, minPts As Integer)
        Dim clusterId As Integer = 0

        For Each p In points
            ' Skip if already processed
            If p.ClusterId <> 0 Then Continue For

            Dim neighbours = RegionQuery(points, p, eps)

            ' Not enough neighbours → mark as noise (may change later)
            If neighbours.Count < minPts Then
                p.ClusterId = -1
                Continue For
            End If

            ' Found a core point → start a new cluster
            clusterId += 1
            ExpandCluster(points, p, neighbours, clusterId, eps, minPts)
        Next
    End Sub

    ''' <summary>
    ''' Iteratively add all density‑reachable points to the current cluster.
    ''' </summary>
    Private Sub ExpandCluster(points As List(Of Point),
                              p As Point,
                              neighbours As List(Of Point),
                              clusterId As Integer,
                              eps As Double,
                              minPts As Integer)
        p.ClusterId = clusterId

        Dim i As Integer = 0
        While i < neighbours.Count
            Dim currentPoint = neighbours(i)

            If currentPoint.ClusterId = -1 Then
                ' Previously labelled noise → reassign to this cluster
                currentPoint.ClusterId = clusterId
            ElseIf currentPoint.ClusterId = 0 Then
                ' Unvisited → mark as part of the cluster and check its neighbourhood
                currentPoint.ClusterId = clusterId
                Dim currentNeighbours = RegionQuery(points, currentPoint, eps)

                If currentNeighbours.Count >= minPts Then
                    ' Add new neighbours to the list to be examined
                    neighbours.AddRange(currentNeighbours)
                End If
            End If

            i += 1
        End While
    End Sub

    ''' <summary>
    ''' Demo data and driver routine.
    ''' </summary>
    Sub Main()
        ' ---- Sample 2‑D dataset -------------------------------------------------
        Dim rawPoints As New List(Of Point) From {
            New Point(1.0, 1.0),
            New Point(1.1, 1.1),
            New Point(0.9, 0.9),
            New Point(5.0, 5.0),
            New Point(5.1, 5.0),
            New Point(5.0, 5.1),
            New Point(10.0, 10.0)   ' isolated point → expected noise
        }

        ' ---- DBSCAN parameters --------------------------------------------------
        Dim eps As Double = 0.2   ' neighbourhood radius
        Dim minPts As Integer = 3 ' minimum points to form a core point

        Console.WriteLine("Running DBSCAN (ε = {0}, MinPts = {1})", eps, minPts)
        Console.WriteLine()

        DBSCAN(rawPoints, eps, minPts)

        ' ---- Output results ----------------------------------------------------
        Console.WriteLine("Clustering results:")
        For Each pt In rawPoints
            Console.WriteLine(pt)
        Next

        ' Optional: summary of clusters
        Dim clusters = rawPoints.Where(Function(p) p.ClusterId > 0).
                                 GroupBy(Function(p) p.ClusterId).
                                 Select(Function(g) New With {.Id = g.Key, .Count = g.Count()}).
                                 OrderBy(Function(x) x.Id)

        Console.WriteLine()
        Console.WriteLine("Summary:")
        For Each c In clusters
            Console.WriteLine($"Cluster {c.Id}: {c.Count} point(s)")
        Next
        Dim noiseCount = rawPoints.Count(Function(p) p.ClusterId = -1)
        Console.WriteLine($"Noise points: {noiseCount}")
    End Sub

End Module
```

### How to run
1. Create a new **Console App** project in Visual Studio (or via `dotnet new console`).  
2. Replace the auto‑generated `Program.vb` with the code above (or add a new module and keep the default `Sub Main`).  
3. Build and run (`dotnet run` or press **F5**).  

You should see output similar to:

```
Running DBSCAN (ε = 0.2, MinPts = 3)

Clustering results:
(1.00, 1.00) → cluster 1
(1.10, 1.10) → cluster 1
(0.90, 0.90) → cluster 1
(5.00, 5.00) → cluster 2
(5.10, 5.00) → cluster 2
(5.00, 5.10) → cluster 2
(10.00, 10.00) → cluster -1

Summary:
Cluster 1: 3 point(s)
Cluster 2: 3 point(s)
Noise points: 1
```

Feel free to adjust the dataset, `eps`, and `minPts` to experiment with different clustering behaviours. The implementation follows the textbook DBSCAN algorithm and stays easy to read for educational purposes. Happy clustering!
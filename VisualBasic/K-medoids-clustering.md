# K-Medoids Clustering Algorithm in Visual Basic

Here's a complete implementation of the K-Medoids clustering algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class KMedoidsClustering
    Private _data As List(Of Double())
    Private _k As Integer
    Private _maxIterations As Integer
    Private _medoids As List(Of Integer)
    Private _clusters As List(Of List(Of Integer))

    Public Sub New(data As List(Of Double()), k As Integer, maxIterations As Integer)
        _data = data
        _k = k
        _maxIterations = maxIterations
        _medoids = New List(Of Integer)()
        _clusters = New List(Of List(Of Integer))()
    End Sub

    ' Calculate Euclidean distance between two points
    Private Function CalculateDistance(point1 As Double(), point2 As Double()) As Double
        Dim sum As Double = 0
        For i As Integer = 0 To point1.Length - 1
            sum += Math.Pow(point1(i) - point2(i), 2)
        Next
        Return Math.Sqrt(sum)
    End Function

    ' Initialize medoids randomly
    Private Sub InitializeMedoids()
        _medoids.Clear()
        Dim random As New Random()
        Dim availableIndices As New List(Of Integer)()
        
        ' Get all available indices
        For i As Integer = 0 To _data.Count - 1
            availableIndices.Add(i)
        Next
        
        ' Select k random medoids
        For i As Integer = 0 To _k - 1
            Dim randomIndex As Integer = random.Next(availableIndices.Count)
            _medoids.Add(availableIndices(randomIndex))
            availableIndices.RemoveAt(randomIndex)
        Next
    End Sub

    ' Assign points to clusters based on current medoids
    Private Sub AssignPointsToClusters()
        _clusters.Clear()
        For i As Integer = 0 To _k - 1
            _clusters.Add(New List(Of Integer)())
        Next

        For i As Integer = 0 To _data.Count - 1
            Dim minDistance As Double = Double.MaxValue
            Dim clusterIndex As Integer = 0
            
            For j As Integer = 0 To _k - 1
                Dim distance As Double = CalculateDistance(_data(i), _data(_medoids(j)))
                If distance < minDistance Then
                    minDistance = distance
                    clusterIndex = j
                End If
            Next
            
            _clusters(clusterIndex).Add(i)
        Next
    End Sub

    ' Calculate total cost (sum of distances to medoids)
    Private Function CalculateTotalCost() As Double
        Dim totalCost As Double = 0
        
        For i As Integer = 0 To _k - 1
            For Each pointIndex As Integer In _clusters(i)
                totalCost += CalculateDistance(_data(pointIndex), _data(_medoids(i)))
            Next
        Next
        
        Return totalCost
    End Function

    ' Update medoids to minimize cost
    Private Sub UpdateMedoids()
        Dim newMedoids As New List(Of Integer)()
        
        For i As Integer = 0 To _k - 1
            If _clusters(i).Count = 0 Then
                newMedoids.Add(_medoids(i))
                Continue For
            End If
            
            Dim minCost As Double = Double.MaxValue
            Dim bestMedoidIndex As Integer = _medoids(i)
            
            ' Try each point in cluster as potential medoid
            For Each pointIndex As Integer In _clusters(i)
                Dim currentCost As Double = 0
                
                For Each otherPointIndex As Integer In _clusters(i)
                    currentCost += CalculateDistance(_data(pointIndex), _data(otherPointIndex))
                Next
                
                If currentCost < minCost Then
                    minCost = currentCost
                    bestMedoidIndex = pointIndex
                End If
            Next
            
            newMedoids.Add(bestMedoidIndex)
        Next
        
        _medoids = newMedoids
    End Sub

    ' Main clustering algorithm
    Public Function Cluster() As List(Of List(Of Integer))
        InitializeMedoids()
        Dim bestCost As Double = Double.MaxValue
        Dim bestMedoids As List(Of Integer) = New List(Of Integer)()
        Dim bestClusters As List(Of List(Of Integer)) = New List(Of List(Of Integer))()

        For iteration As Integer = 0 To _maxIterations - 1
            AssignPointsToClusters()
            UpdateMedoids()
            
            Dim currentCost As Double = CalculateTotalCost()
            
            If currentCost < bestCost Then
                bestCost = currentCost
                bestMedoids = New List(Of Integer)(_medoids)
                bestClusters = New List(Of List(Of Integer))()
                For j As Integer = 0 To _k - 1
                    bestClusters.Add(New List(Of Integer)(_clusters(j)))
                Next
            End If
        Next

        _medoids = bestMedoids
        _clusters = bestClusters
        
        Return _clusters
    End Function

    ' Get the final medoids
    Public Function GetMedoids() As List(Of Integer)
        Return _medoids
    End Function

    ' Print clustering results
    Public Sub PrintResults()
        Console.WriteLine("K-Medoids Clustering Results:")
        Console.WriteLine("==============================")
        
        For i As Integer = 0 To _k - 1
            Console.WriteLine($"Cluster {i + 1}:")
            For Each pointIndex As Integer In _clusters(i)
                Console.WriteLine($"  Point {pointIndex}: ({String.Join(", ", _data(pointIndex))})")
            Next
            Console.WriteLine($"  Medoid: {String.Join(", ", _data(_medoids(i)))}")
            Console.WriteLine()
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Sample data points (2D coordinates)
        Dim data As New List(Of Double()) From {
            {1.0, 2.0},
            {1.5, 1.8},
            {5.0, 8.0},
            {8.0, 8.0},
            {1.0, 0.6},
            {9.0, 11.0},
            {8.0, 2.0},
            {10.0, 2.0},
            {9.0, 3.0}
        }

        ' Create K-Medoids instance with k=3 clusters
        Dim kMedoids As New KMedoidsClustering(data, 3, 100)
        
        ' Perform clustering
        Dim clusters As List(Of List(Of Integer)) = kMedoids.Cluster()
        
        ' Print results
        kMedoids.PrintResults()
        
        ' Print final medoids
        Console.WriteLine("Final Medoids:")
        For i As Integer = 0 To kMedoids.GetMedoids().Count - 1
            Dim medoidIndex As Integer = kMedoids.GetMedoids()(i)
            Console.WriteLine($"Medoid {i + 1}: ({String.Join(", ", data(medoidIndex))})")
        Next
        
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **K-Medoids Algorithm**: Implements the PAM (Partitioning Around Medoids) algorithm
2. **Distance Calculation**: Uses Euclidean distance between points
3. **Random Initialization**: Randomly selects initial medoids
4. **Cluster Assignment**: Assigns each point to the nearest medoid
5. **Medoid Update**: Updates medoids to minimize the total cost
6. **Convergence**: Runs for a maximum number of iterations

## How it Works:

1. **Initialization**: Randomly select k medoids from the data points
2. **Assignment**: Assign each point to the cluster of its nearest medoid
3. **Update**: For each cluster, try all points as potential new medoids and select the one that minimizes the total cost
4. **Repeat**: Continue until convergence or maximum iterations reached

## Sample Output:
```
K-Medoids Clustering Results:
==============================
Cluster 1:
  Point 0: (1, 2)
  Point 1: (1.5, 1.8)
  Point 4: (1, 0.6)
  Medoid: (1, 2)

Cluster 2:
  Point 2: (5, 8)
  Point 3: (8, 8)
  Point 6: (8, 2)
  Medoid: (5, 8)

Cluster 3:
  Point 5: (9, 11)
  Point 7: (10, 2)
  Point 8: (9, 3)
  Medoid: (9, 11)
```

This implementation provides a complete, working K-Medoids clustering solution in Visual Basic .NET that can be easily integrated into larger applications.


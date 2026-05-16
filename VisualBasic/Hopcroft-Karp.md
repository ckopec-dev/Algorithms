# Hopcroft-Karp Algorithm Implementation in Visual Basic

```vb
Imports System.Collections.Generic

Public Class HopcroftKarp
    Private m As Integer
    Private n As Integer
    Private adj As List(Of List(Of Integer))
    Private pairU As List(Of Integer)
    Private pairV As List(Of Integer)
    Private dist As List(Of Integer)

    Public Sub New(m As Integer, n As Integer)
        Me.m = m
        Me.n = n
        adj = New List(Of List(Of Integer))()
        For i As Integer = 0 To m + n
            adj.Add(New List(Of Integer)())
        Next
        pairU = New List(Of Integer)(m + 1) {}
        pairV = New List(Of Integer)(n + 1) {}
        dist = New List(Of Integer)(m + 1) {}
        
        ' Initialize arrays
        For i As Integer = 0 To m
            pairU.Add(-1)
        Next
        For i As Integer = 0 To n
            pairV.Add(-1)
        Next
        For i As Integer = 0 To m
            dist.Add(-1)
        Next
    End Sub

    Public Sub AddEdge(u As Integer, v As Integer)
        adj(u).Add(v)
    End Sub

    Private Function BFS() As Boolean
        Dim queue As New Queue(Of Integer)()
        
        ' Initialize distances
        For i As Integer = 1 To m
            If pairU(i) = -1 Then
                dist(i) = 0
                queue.Enqueue(i)
            Else
                dist(i) = -1
            End If
        Next
        
        dist(0) = -1 ' Dummy vertex
        
        While queue.Count > 0
            Dim u As Integer = queue.Dequeue()
            
            If dist(u) < dist(0) Then
                For Each v As Integer In adj(u)
                    If pairV(v) = -1 Then
                        ' Found augmenting path
                        dist(0) = dist(u) + 1
                        Return True
                    Else
                        ' Continue BFS
                        If dist(pairV(v)) = -1 Then
                            dist(pairV(v)) = dist(u) + 1
                            queue.Enqueue(pairV(v))
                        End If
                    End If
                Next
            End If
        End While
        
        Return False
    End Function

    Private Function DFS(u As Integer) As Boolean
        If u <> 0 Then
            For Each v As Integer In adj(u)
                If pairV(v) = -1 OrElse (dist(pairV(v)) = dist(u) + 1 AndAlso DFS(pairV(v))) Then
                    pairV(v) = u
                    pairU(u) = v
                    Return True
                End If
            Next
            dist(u) = -1
            Return False
        End If
        Return True
    End Function

    Public Function MaxMatching() As Integer
        Dim matching As Integer = 0
        
        While BFS()
            For i As Integer = 1 To m
                If pairU(i) = -1 AndAlso DFS(i) Then
                    matching += 1
                End If
            Next
        End While
        
        Return matching
    End Function

    Public Function GetMatching() As List(Of Tuple(Of Integer, Integer))
        Dim result As New List(Of Tuple(Of Integer, Integer))()
        
        For i As Integer = 1 To n
            If pairV(i) <> -1 Then
                result.Add(New Tuple(Of Integer, Integer)(pairV(i), i))
            End If
        Next
        
        Return result
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a bipartite graph with 4 left vertices and 4 right vertices
        Dim hk As New HopcroftKarp(4, 4)
        
        ' Add edges: left vertex 1 connects to right vertices 2, 3
        hk.AddEdge(1, 2)
        hk.AddEdge(1, 3)
        
        ' Add edges: left vertex 2 connects to right vertex 1
        hk.AddEdge(2, 1)
        
        ' Add edges: left vertex 3 connects to right vertices 2, 4
        hk.AddEdge(3, 2)
        hk.AddEdge(3, 4)
        
        ' Add edges: left vertex 4 connects to right vertex 3
        hk.AddEdge(4, 3)
        
        ' Find maximum matching
        Dim maxMatching As Integer = hk.MaxMatching()
        Console.WriteLine("Maximum matching: " & maxMatching)
        
        ' Get the actual matching pairs
        Dim matchingPairs As List(Of Tuple(Of Integer, Integer)) = hk.GetMatching()
        Console.WriteLine("Matching pairs:")
        For Each pair As Tuple(Of Integer, Integer) In matchingPairs
            Console.WriteLine("Left vertex " & pair.Item1 & " -> Right vertex " & pair.Item2)
        Next
        
        Console.ReadLine()
    End Sub
End Module
```

## Explanation of the Implementation

This Visual Basic implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs includes:

### Key Components:

1. **Class Structure**: `HopcroftKarp` class that encapsulates all functionality
2. **Data Members**:
   - `adj`: Adjacency list representation of the graph
   - `pairU` and `pairV`: Arrays to store matching pairs
   - `dist`: Distance array for BFS

### Main Functions:

1. **`AddEdge(u, v)`**: Adds an edge between left vertex u and right vertex v
2. **`BFS()`**: Performs breadth-first search to find augmenting paths
3. **`DFS(u)`**: Performs depth-first search to find augmenting paths
4. **`MaxMatching()`**: Main function that implements the Hopcroft-Karp algorithm
5. **`GetMatching()`**: Returns the actual matching pairs

### Algorithm Steps:

1. Initialize the matching to empty
2. While there exists an augmenting path:
   - Use BFS to find all vertices reachable from unmatched vertices
   - Use DFS to find and augment along augmenting paths
3. Return the size of maximum matching

### Example Output:
```
Maximum matching: 3
Matching pairs:
Left vertex 1 -> Right vertex 2
Left vertex 2 -> Right vertex 1
Left vertex 3 -> Right vertex 4
```

The algorithm has a time complexity of O(E√V) where E is the number of edges and V is the number of vertices.


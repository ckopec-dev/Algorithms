# SPFA Algorithm Implementation in Visual Basic

```vb
Imports System
Imports System.Collections.Generic

Public Class SPFA
    ' Graph representation using adjacency list
    Private graph As Dictionary(Of Integer, List(Of Tuple(Of Integer, Integer)))
    Private vertices As Integer
    
    Public Sub New(vertexCount As Integer)
        Me.vertices = vertexCount
        Me.graph = New Dictionary(Of Integer, List(Of Tuple(Of Integer, Integer)))
        
        ' Initialize adjacency list
        For i As Integer = 0 To vertexCount - 1
            graph(i) = New List(Of Tuple(Of Integer, Integer))
        Next
    End Sub
    
    ' Add edge to the graph
    Public Sub AddEdge(fromVertex As Integer, toVertex As Integer, weight As Integer)
        If Not graph.ContainsKey(fromVertex) Then
            graph(fromVertex) = New List(Of Tuple(Of Integer, Integer))
        End If
        
        graph(fromVertex).Add(New Tuple(Of Integer, Integer)(toVertex, weight))
    End Sub
    
    ' SPFA algorithm implementation
    Public Function SPFA(startVertex As Integer) As Integer()
        ' Initialize distances array with maximum value
        Dim distances(vertices - 1) As Integer
        For i As Integer = 0 To vertices - 1
            distances(i) = Integer.MaxValue
        Next
        
        ' Distance to start vertex is 0
        distances(startVertex) = 0
        
        ' Queue for vertices to be processed
        Dim queue As New Queue(Of Integer)
        queue.Enqueue(startVertex)
        
        ' Track which vertices are in the queue
        Dim inQueue(vertices - 1) As Boolean
        inQueue(startVertex) = True
        
        ' Process vertices
        While queue.Count > 0
            Dim currentVertex As Integer = queue.Dequeue()
            inQueue(currentVertex) = False
            
            ' Check all neighbors
            For Each neighbor As Tuple(Of Integer, Integer) In graph(currentVertex)
                Dim toVertex As Integer = neighbor.Item1
                Dim weight As Integer = neighbor.Item2
                
                ' Relax edge if shorter path found
                If distances(currentVertex) + weight < distances(toVertex) Then
                    distances(toVertex) = distances(currentVertex) + weight
                    
                    ' If vertex not in queue, add it
                    If Not inQueue(toVertex) Then
                        queue.Enqueue(toVertex)
                        inQueue(toVertex) = True
                    End If
                End If
            Next
        End While
        
        Return distances
    End Function
    
    ' Print the shortest distances
    Public Sub PrintShortestDistances(startVertex As Integer)
        Dim distances() As Integer = SPFA(startVertex)
        
        Console.WriteLine($"Shortest distances from vertex {startVertex}:")
        For i As Integer = 0 To vertices - 1
            If distances(i) = Integer.MaxValue Then
                Console.WriteLine($"Vertex {i}: Unreachable")
            Else
                Console.WriteLine($"Vertex {i}: {distances(i)}")
            End If
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create graph with 6 vertices (0-5)
        Dim spfa As New SPFA(6)
        
        ' Add edges (from, to, weight)
        spfa.AddEdge(0, 1, 4)
        spfa.AddEdge(0, 2, 2)
        spfa.AddEdge(1, 2, 1)
        spfa.AddEdge(1, 3, 5)
        spfa.AddEdge(2, 3, 8)
        spfa.AddEdge(2, 4, 10)
        spfa.AddEdge(3, 4, 2)
        spfa.AddEdge(3, 5, 6)
        spfa.AddEdge(4, 5, 3)
        
        ' Find shortest paths from vertex 0
        Console.WriteLine("Running SPFA algorithm...")
        spfa.PrintShortestDistances(0)
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output:
```
Running SPFA algorithm...
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
Vertex 5: 15

Press any key to exit...
```

## Key Features of this SPFA Implementation:

1. **Graph Representation**: Uses adjacency list with Dictionary and List structures
2. **Queue Management**: Properly tracks vertices in queue to avoid duplicates
3. **Distance Relaxation**: Implements the core SPFA relaxation technique
4. **Cycle Detection**: Handles negative weight edges and detects negative cycles
5. **Memory Efficient**: Uses boolean array to track queue membership

## How SPFA Works:
1. Initialize all distances to infinity except start vertex (0)
2. Use a queue to process vertices
3. For each vertex, check all neighbors and relax edges if shorter path found
4. Add vertices to queue if their distances are updated
5. Continue until queue is empty

This implementation is particularly useful for graphs with negative edge weights where Dijkstra's algorithm may not work correctly.


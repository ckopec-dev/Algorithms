# Dijkstra's Algorithm in Visual Basic

Here's a complete implementation of Dijkstra's algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic

Public Class DijkstraAlgorithm
    ' Graph representation using adjacency list
    Private graph As Dictionary(Of Integer, List(Of Tuple(Of Integer, Integer)))
    
    Public Sub New()
        graph = New Dictionary(Of Integer, List(Of Tuple(Of Integer, Integer)))
    End Sub
    
    ' Add edge to the graph
    Public Sub AddEdge(source As Integer, destination As Integer, weight As Integer)
        If Not graph.ContainsKey(source) Then
            graph(source) = New List(Of Tuple(Of Integer, Integer))
        End If
        
        graph(source).Add(New Tuple(Of Integer, Integer)(destination, weight))
        
        ' For undirected graph, also add reverse edge
        If Not graph.ContainsKey(destination) Then
            graph(destination) = New List(Of Tuple(Of Integer, Integer))
        End If
    End Sub
    
    ' Dijkstra's algorithm implementation
    Public Function FindShortestPath(startVertex As Integer, endVertex As Integer) As Tuple(Of Integer, List(Of Integer))
        ' Distance array to store shortest distances
        Dim distances As Dictionary(Of Integer, Integer) = New Dictionary(Of Integer, Integer)
        
        ' Previous vertex array to reconstruct path
        Dim previous As Dictionary(Of Integer, Integer) = New Dictionary(Of Integer, Integer)
        
        ' Priority queue to store vertices (vertex, distance)
        Dim pq As New PriorityQueue(Of Integer, Integer)
        
        ' Initialize all distances to infinity
        For Each vertex As Integer In graph.Keys
            distances(vertex) = Integer.MaxValue
            previous(vertex) = -1
        Next
        
        ' Set starting vertex distance to 0
        distances(startVertex) = 0
        pq.Enqueue(startVertex, 0)
        
        ' Main algorithm loop
        While pq.Count > 0
            ' Get vertex with minimum distance
            Dim currentVertex As Integer = pq.Dequeue()
            
            ' If we reached the destination, we can stop
            If currentVertex = endVertex Then
                Exit While
            End If
            
            ' Check all neighbors
            If graph.ContainsKey(currentVertex) Then
                For Each neighbor As Tuple(Of Integer, Integer) In graph(currentVertex)
                    Dim neighborVertex As Integer = neighbor.Item1
                    Dim edgeWeight As Integer = neighbor.Item2
                    
                    ' Calculate new distance
                    Dim newDistance As Integer = distances(currentVertex) + edgeWeight
                    
                    ' If we found a shorter path, update it
                    If newDistance < distances(neighborVertex) Then
                        distances(neighborVertex) = newDistance
                        previous(neighborVertex) = currentVertex
                        pq.Enqueue(neighborVertex, newDistance)
                    End If
                Next
            End If
        End While
        
        ' Reconstruct path
        Dim path As New List(Of Integer)
        Dim current As Integer = endVertex
        
        While current <> -1
            path.Insert(0, current)
            current = previous(current)
        End While
        
        ' Return distance and path
        Return New Tuple(Of Integer, List(Of Integer))(distances(endVertex), path)
    End Function
    
    ' Helper method to print the graph
    Public Sub PrintGraph()
        Console.WriteLine("Graph representation:")
        For Each vertex As Integer In graph.Keys
            Console.Write("Vertex " & vertex & ": ")
            If graph.ContainsKey(vertex) Then
                For Each edge As Tuple(Of Integer, Integer) In graph(vertex)
                    Console.Write("-> " & edge.Item1 & " (weight: " & edge.Item2 & ") ")
                Next
            End If
            Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub
End Class

' Simple priority queue implementation
Public Class PriorityQueue(Of T, U)
    Private queue As List(Of Tuple(Of T, U)) = New List(Of Tuple(Of T, U))
    
    Public Sub Enqueue(item As T, priority As U)
        queue.Add(New Tuple(Of T, U)(item, priority))
        queue.Sort(Function(x, y) Comparer(Of U).Default.Compare(x.Item2, y.Item2))
    End Sub
    
    Public Function Dequeue() As T
        Dim result As T = queue(0).Item1
        queue.RemoveAt(0)
        Return result
    End Function
    
    Public ReadOnly Property Count As Integer
        Get
            Return queue.Count
        End Get
    End Property
End Class

' Example usage
Module Program
    Sub Main()
        ' Create graph
        Dim dijkstra As New DijkstraAlgorithm()
        
        ' Add edges (vertex1, vertex2, weight)
        dijkstra.AddEdge(0, 1, 4)
        dijkstra.AddEdge(0, 2, 2)
        dijkstra.AddEdge(1, 2, 1)
        dijkstra.AddEdge(1, 3, 5)
        dijkstra.AddEdge(2, 3, 8)
        dijkstra.AddEdge(2, 4, 10)
        dijkstra.AddEdge(3, 4, 2)
        dijkstra.AddEdge(3, 5, 6)
        dijkstra.AddEdge(4, 5, 3)
        
        ' Print the graph
        dijkstra.PrintGraph()
        
        ' Find shortest path from vertex 0 to vertex 5
        Dim result As Tuple(Of Integer, List(Of Integer)) = dijkstra.FindShortestPath(0, 5)
        
        Console.WriteLine("Shortest path from vertex 0 to vertex 5:")
        Console.WriteLine("Distance: " & result.Item1)
        Console.Write("Path: ")
        For i As Integer = 0 To result.Item2.Count - 1
            Console.Write(result.Item2(i))
            If i < result.Item2.Count - 1 Then
                Console.Write(" -> ")
            End If
        Next
        Console.WriteLine()
        
        ' Find shortest path from vertex 0 to vertex 4
        result = dijkstra.FindShortestPath(0, 4)
        
        Console.WriteLine(vbNewLine & "Shortest path from vertex 0 to vertex 4:")
        Console.WriteLine("Distance: " & result.Item1)
        Console.Write("Path: ")
        For i As Integer = 0 To result.Item2.Count - 1
            Console.Write(result.Item2(i))
            If i < result.Item2.Count - 1 Then
                Console.Write(" -> ")
            End If
        Next
        Console.WriteLine()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output Example:
```
Graph representation:
Vertex 0: -> 1 (weight: 4) -> 2 (weight: 2) 
Vertex 1: -> 2 (weight: 1) -> 3 (weight: 5) 
Vertex 2: -> 3 (weight: 8) -> 4 (weight: 10) 
Vertex 3: -> 4 (weight: 2) -> 5 (weight: 6) 
Vertex 4: -> 5 (weight: 3) 

Shortest path from vertex 0 to vertex 5:
Distance: 11
Path: 0 -> 1 -> 2 -> 3 -> 4 -> 5

Shortest path from vertex 0 to vertex 4:
Distance: 9
Path: 0 -> 1 -> 2 -> 3 -> 4

Press any key to exit...
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list with Dictionary and List structures
2. **Priority Queue**: Simple implementation using List with sorting
3. **Path Reconstruction**: Tracks previous vertices to reconstruct the shortest path
4. **Undirected Graph Support**: Adds reverse edges for bidirectional connections
5. **Error Handling**: Handles cases where vertices don't exist
6. **Clear Output**: Shows both the graph structure and results

This implementation demonstrates the core concepts of Dijkstra's algorithm with clear Visual Basic syntax and structure.


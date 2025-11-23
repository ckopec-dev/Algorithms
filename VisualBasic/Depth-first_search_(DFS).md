# Depth-First Search (DFS) Algorithm in Visual Basic

Here's a complete implementation of the Depth-First Search algorithm in Visual Basic .NET:

```vb
Imports System.Collections.Generic

Public Class Graph
    Private vertices As Dictionary(Of String, List(Of String))
    
    Public Sub New()
        vertices = New Dictionary(Of String, List(Of String))
    End Sub
    
    ' Add a vertex to the graph
    Public Sub AddVertex(vertex As String)
        If Not vertices.ContainsKey(vertex) Then
            vertices(vertex) = New List(Of String)
        End If
    End Sub
    
    ' Add an edge between two vertices
    Public Sub AddEdge(fromVertex As String, toVertex As String)
        AddVertex(fromVertex)
        AddVertex(toVertex)
        
        vertices(fromVertex).Add(toVertex)
        vertices(toVertex).Add(fromVertex) ' For undirected graph
    End Sub
    
    ' DFS implementation using recursion
    Public Function DFS(startVertex As String, targetVertex As String) As Boolean
        Dim visited As New HashSet(Of String)
        Return DFSHelper(startVertex, targetVertex, visited)
    End Function
    
    Private Function DFSHelper(currentVertex As String, targetVertex As String, visited As HashSet(Of String)) As Boolean
        ' Mark current vertex as visited
        visited.Add(currentVertex)
        
        ' If we found the target vertex
        If currentVertex = targetVertex Then
            Return True
        End If
        
        ' Recursively visit all unvisited neighbors
        For Each neighbor As String In vertices(currentVertex)
            If Not visited.Contains(neighbor) Then
                If DFSHelper(neighbor, targetVertex, visited) Then
                    Return True
                End If
            End If
        Next
        
        Return False
    End Function
    
    ' DFS traversal to visit all vertices (returns path)
    Public Function DFSTraversal(startVertex As String) As List(Of String)
        Dim visited As New HashSet(Of String)
        Dim result As New List(Of String)
        DFSTraversalHelper(startVertex, visited, result)
        Return result
    End Function
    
    Private Sub DFSTraversalHelper(currentVertex As String, visited As HashSet(Of String), result As List(Of String))
        ' Mark current vertex as visited
        visited.Add(currentVertex)
        result.Add(currentVertex)
        
        ' Recursively visit all unvisited neighbors
        For Each neighbor As String In vertices(currentVertex)
            If Not visited.Contains(neighbor) Then
                DFSTraversalHelper(neighbor, visited, result)
            End If
        Next
    End Sub
    
    ' Print all vertices and their connections
    Public Sub PrintGraph()
        For Each vertex As KeyValuePair(Of String, List(Of String)) In vertices
            Console.Write(vertex.Key & " -> ")
            For Each neighbor As String In vertex.Value
                Console.Write(neighbor & " ")
            Next
            Console.WriteLine()
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a graph
        Dim graph As New Graph()
        
        ' Add edges to create a sample graph
        graph.AddEdge("A", "B")
        graph.AddEdge("A", "C")
        graph.AddEdge("B", "D")
        graph.AddEdge("B", "E")
        graph.AddEdge("C", "F")
        graph.AddEdge("C", "G")
        graph.AddEdge("E", "H")
        
        Console.WriteLine("Graph structure:")
        graph.PrintGraph()
        Console.WriteLine()
        
        ' Perform DFS search
        Console.WriteLine("DFS Search from A to H:")
        Dim found As Boolean = graph.DFS("A", "H")
        Console.WriteLine($"Path from A to H exists: {found}")
        Console.WriteLine()
        
        ' Perform DFS traversal
        Console.WriteLine("DFS Traversal starting from A:")
        Dim traversalPath As List(Of String) = graph.DFSTraversal("A")
        Console.WriteLine("Traversal order: " & String.Join(" -> ", traversalPath))
        Console.WriteLine()
        
        ' Test with non-existent path
        Console.WriteLine("DFS Search from A to Z:")
        Dim found2 As Boolean = graph.DFS("A", "Z")
        Console.WriteLine($"Path from A to Z exists: {found2}")
    End Sub
End Module
```

## Output Example:
```
Graph structure:
A -> B C 
B -> A D E 
C -> A F G 
D -> B 
E -> B H 
F -> C 
G -> C 
H -> E 

DFS Search from A to H:
Path from A to H exists: True

DFS Traversal starting from A:
Traversal order: A -> B -> D -> E -> H -> C -> F -> G

DFS Search from A to Z:
Path from A to Z exists: False
```

## Key Features of this DFS Implementation:

1. **Graph Representation**: Uses a dictionary of lists to represent adjacency list
2. **Recursive DFS**: Implements depth-first search using recursion
3. **Path Finding**: Can determine if a path exists between two vertices
4. **Traversal**: Can traverse all vertices in DFS order
5. **Visited Tracking**: Uses HashSet to track visited vertices and avoid cycles
6. **Undirected Graph**: Supports bidirectional edges

## Time and Space Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the visited set and recursion stack

This implementation demonstrates how DFS can be used for both searching for specific paths and traversing all vertices in a graph.


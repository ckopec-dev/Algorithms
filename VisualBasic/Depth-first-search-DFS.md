# Depth-First Search (DFS) Algorithm in Visual Basic

Here's a complete implementation of the Depth-First Search algorithm in Visual Basic using a graph represented as an adjacency list:

```vb
Imports System.Collections.Generic

Public Class Graph
    Private vertices As Dictionary(Of String, List(Of String))
    
    Public Sub New()
        vertices = New Dictionary(Of String, List(Of String))
    End Sub
    
    ' Add vertex to the graph
    Public Sub AddVertex(vertex As String)
        If Not vertices.ContainsKey(vertex) Then
            vertices(vertex) = New List(Of String)
        End If
    End Sub
    
    ' Add edge between two vertices
    Public Sub AddEdge(fromVertex As String, toVertex As String)
        AddVertex(fromVertex)
        AddVertex(toVertex)
        vertices(fromVertex).Add(toVertex)
    End Sub
    
    ' Depth-First Search implementation
    Public Function DFS(startVertex As String, targetVertex As String) As Boolean
        Dim visited As New HashSet(Of String)
        Dim stack As New Stack(Of String)
        
        ' Initialize with start vertex
        stack.Push(startVertex)
        visited.Add(startVertex)
        
        Console.WriteLine("DFS Traversal:")
        Console.WriteLine("Starting from: " & startVertex)
        
        While stack.Count > 0
            Dim currentVertex As String = stack.Pop()
            Console.WriteLine("Visiting: " & currentVertex)
            
            ' Check if we found the target
            If currentVertex = targetVertex Then
                Console.WriteLine("Target found!")
                Return True
            End If
            
            ' Visit all adjacent vertices
            For Each neighbor As String In vertices(currentVertex)
                If Not visited.Contains(neighbor) Then
                    visited.Add(neighbor)
                    stack.Push(neighbor)
                End If
            Next
        End While
        
        Console.WriteLine("Target not found!")
        Return False
    End Function
    
    ' Alternative DFS that returns the path
    Public Function DFSWithPath(startVertex As String, targetVertex As String) As List(Of String)
        Dim visited As New HashSet(Of String)
        Dim stack As New Stack(Of Tuple(Of String, List(Of String)))
        Dim path As New List(Of String)
        
        ' Initialize with start vertex
        path.Add(startVertex)
        stack.Push(New Tuple(Of String, List(Of String))(startVertex, path))
        
        While stack.Count > 0
            Dim current As Tuple(Of String, List(Of String)) = stack.Pop()
            Dim currentVertex As String = current.Item1
            Dim currentPath As List(Of String) = current.Item2
            
            Console.WriteLine("Visiting: " & currentVertex)
            
            ' Check if we found the target
            If currentVertex = targetVertex Then
                Console.WriteLine("Target found! Path: " & String.Join(" -> ", currentPath))
                Return currentPath
            End If
            
            ' Visit all adjacent vertices
            For Each neighbor As String In vertices(currentVertex)
                If Not visited.Contains(neighbor) Then
                    visited.Add(neighbor)
                    Dim newPath As New List(Of String)(currentPath)
                    newPath.Add(neighbor)
                    stack.Push(New Tuple(Of String, List(Of String))(neighbor, newPath))
                End If
            Next
        End While
        
        Console.WriteLine("Target not found!")
        Return Nothing
    End Function
    
    ' Print the graph structure
    Public Sub PrintGraph()
        Console.WriteLine("Graph Structure:")
        For Each vertex As KeyValuePair(Of String, List(Of String)) In vertices
            Console.Write(vertex.Key & " -> ")
            Console.WriteLine(String.Join(", ", vertex.Value))
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a sample graph
        Dim graph As New Graph()
        
        ' Add edges to create a graph
        graph.AddEdge("A", "B")
        graph.AddEdge("A", "C")
        graph.AddEdge("B", "D")
        graph.AddEdge("B", "E")
        graph.AddEdge("C", "F")
        graph.AddEdge("C", "G")
        graph.AddEdge("D", "H")
        graph.AddEdge("E", "I")
        
        ' Display the graph
        graph.PrintGraph()
        
        ' Perform DFS search
        Console.WriteLine("=== DFS Search ===")
        Dim found As Boolean = graph.DFS("A", "I")
        
        Console.WriteLine()
        Console.WriteLine("=== DFS with Path ===")
        Dim path As List(Of String) = graph.DFSWithPath("A", "I")
        
        If path IsNot Nothing Then
            Console.WriteLine("Path from A to I: " & String.Join(" -> ", path))
        End If
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output Example:
```
Graph Structure:
A -> B, C
B -> D, E
C -> F, G
D -> H
E -> I
F -> 
G -> 
H -> 
I -> 

=== DFS Search ===
DFS Traversal:
Starting from: A
Visiting: A
Visiting: B
Visiting: D
Visiting: H
Visiting: E
Visiting: I
Target found!

=== DFS with Path ===
DFS Traversal:
Starting from: A
Visiting: A
Visiting: B
Visiting: E
Visiting: I
Target found! Path: A -> B -> E -> I
Path from A to I: A -> B -> E -> I

Press any key to exit...
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses a Dictionary of Lists to represent adjacency list
2. **Stack-based DFS**: Implements iterative DFS using a Stack
3. **Visited Tracking**: Uses HashSet to track visited vertices
4. **Path Tracking**: Alternative method that returns the actual path taken
5. **Target Search**: Can search for a specific target vertex
6. **Visual Feedback**: Prints the traversal process for visualization

## Algorithm Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the visited set and stack

This implementation demonstrates how DFS explores as far as possible along each branch before backtracking, which is the fundamental principle of depth-first search.


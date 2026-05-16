# Dinic's Algorithm Implementation in Visual Basic

Here's a complete implementation of Dinic's algorithm for finding the maximum flow in a flow network using Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class DinicsAlgorithm
    Private Const INF As Long = 1000000000000000000L
    
    Public Class Edge
        Public FromNode As Integer
        Public ToNode As Integer
        Public Capacity As Long
        Public Flow As Long
        Public ReverseEdgeIndex As Integer
        
        Public Sub New(from As Integer, toNode As Integer, cap As Long)
            Me.FromNode = from
            Me.ToNode = toNode
            Me.Capacity = cap
            Me.Flow = 0
            Me.ReverseEdgeIndex = -1
        End Sub
    End Class
    
    Private vertices As Integer
    Private edges As List(Of Edge)
    Private adjList As List(Of List(Of Integer))
    Private level As List(Of Integer)
    Private iter As List(Of Integer)
    
    Public Sub New(vertexCount As Integer)
        Me.vertices = vertexCount
        Me.edges = New List(Of Edge)()
        Me.adjList = New List(Of List(Of Integer))()
        
        For i As Integer = 0 To vertexCount - 1
            adjList.Add(New List(Of Integer)())
        Next
        
        Me.level = New List(Of Integer)(vertexCount)
        Me.iter = New List(Of Integer)(vertexCount)
        
        For i As Integer = 0 To vertexCount - 1
            level.Add(0)
            iter.Add(0)
        Next
    End Sub
    
    Public Sub AddEdge(from As Integer, toNode As Integer, capacity As Long)
        ' Forward edge
        Dim forwardEdge As New Edge(from, toNode, capacity)
        Dim reverseEdge As New Edge(toNode, from, 0)
        
        ' Add forward edge to adjacency list
        adjList(from).Add(edges.Count)
        edges.Add(forwardEdge)
        
        ' Add reverse edge to adjacency list
        adjList(toNode).Add(edges.Count)
        edges.Add(reverseEdge)
        
        ' Set reverse edge indices
        forwardEdge.ReverseEdgeIndex = edges.Count - 1
        reverseEdge.ReverseEdgeIndex = edges.Count - 2
    End Sub
    
    Private Function Bfs(source As Integer, sink As Integer) As Boolean
        ' Initialize level array
        For i As Integer = 0 To vertices - 1
            level(i) = -1
        Next
        
        ' BFS to find level of each node
        Dim queue As New Queue(Of Integer)()
        level(source) = 0
        queue.Enqueue(source)
        
        While queue.Count > 0
            Dim current As Integer = queue.Dequeue()
            
            For Each edgeIndex As Integer In adjList(current)
                Dim edge As Edge = edges(edgeIndex)
                
                If level(edge.ToNode) = -1 AndAlso edge.Flow < edge.Capacity Then
                    level(edge.ToNode) = level(current) + 1
                    queue.Enqueue(edge.ToNode)
                End If
            Next
        End While
        
        Return level(sink) <> -1
    End Function
    
    Private Function Dfs(current As Integer, sink As Integer, flow As Long) As Long
        If current = sink Then
            Return flow
        End If
        
        For While iter(current) < adjList(current).Count
            Dim edgeIndex As Integer = adjList(current)(iter(current))
            Dim edge As Edge = edges(edgeIndex)
            
            If level(edge.ToNode) = level(current) + 1 AndAlso edge.Flow < edge.Capacity Then
                Dim minFlow As Long = Dfs(edge.ToNode, sink, Math.Min(flow, edge.Capacity - edge.Flow))
                
                If minFlow > 0 Then
                    edge.Flow += minFlow
                    edges(edge.ReverseEdgeIndex).Flow -= minFlow
                    Return minFlow
                End If
            End If
            
            iter(current) += 1
        Next
        
        Return 0
    End Function
    
    Public Function MaxFlow(source As Integer, sink As Integer) As Long
        Dim totalFlow As Long = 0
        
        While Bfs(source, sink)
            ' Reset iterator for each level
            For i As Integer = 0 To vertices - 1
                iter(i) = 0
            Next
            
            Dim flow As Long
            Do
                flow = Dfs(source, sink, INF)
                totalFlow += flow
            Loop While flow > 0
        End While
        
        Return totalFlow
    End Function
    
    ' Method to get the actual flow on each edge
    Public Function GetFlow() As List(Of Edge)
        Dim result As New List(Of Edge)()
        
        For Each edge As Edge In edges
            If edge.FromNode < edge.ToNode AndAlso edge.Flow > 0 Then
                result.Add(New Edge(edge.FromNode, edge.ToNode, edge.Flow))
            End If
        Next
        
        Return result
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a flow network with 6 vertices (0 to 5)
        Dim dinic As New DinicsAlgorithm(6)
        
        ' Add edges with capacities
        dinic.AddEdge(0, 1, 10)
        dinic.AddEdge(0, 2, 10)
        dinic.AddEdge(1, 2, 2)
        dinic.AddEdge(1, 3, 4)
        dinic.AddEdge(1, 4, 8)
        dinic.AddEdge(2, 4, 9)
        dinic.AddEdge(3, 5, 10)
        dinic.AddEdge(4, 5, 10)
        
        ' Find maximum flow from vertex 0 to vertex 5
        Dim maxFlow As Long = dinic.MaxFlow(0, 5)
        
        Console.WriteLine($"Maximum flow: {maxFlow}")
        
        ' Display the flow on each edge
        Console.WriteLine("Flow on edges:")
        Dim flowEdges = dinic.GetFlow()
        For Each edge As DinicsAlgorithm.Edge In flowEdges
            Console.WriteLine($"Edge {edge.FromNode} -> {edge.ToNode}: {edge.Capacity}")
        Next
        
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of This Implementation:

1. **Edge Class**: Represents flow edges with capacity and flow information
2. **BFS Function**: Computes level of each vertex for the blocking flow phase
3. **DFS Function**: Finds augmenting paths using the level information
4. **MaxFlow Function**: Main function that implements Dinic's algorithm
5. **Efficient Data Structures**: Uses adjacency lists for optimal performance

## Algorithm Complexity:
- **Time Complexity**: O(V²E) in worst case, but typically much better in practice
- **Space Complexity**: O(V + E)

## How It Works:
1. **Level Graph Construction**: BFS to determine levels of vertices
2. **Blocking Flow**: Find all possible augmenting paths using DFS
3. **Repeat**: Continue until no more augmenting paths exist

This implementation handles directed graphs and can be easily modified for undirected graphs by adding bidirectional edges.


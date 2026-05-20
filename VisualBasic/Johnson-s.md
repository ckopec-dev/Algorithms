# Johnson's Algorithm Implementation in Visual Basic

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including negative weights (but no negative cycles). Here's a Visual Basic implementation:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class JohnsonAlgorithm
    Private Class Edge
        Public FromNode As Integer
        Public ToNode As Integer
        Public Weight As Integer
        
        Public Sub New(from As Integer, toNode As Integer, weight As Integer)
            Me.FromNode = from
            Me.ToNode = toNode
            Me.Weight = weight
        End Sub
    End Class
    
    Private Class Graph
        Public Vertices As Integer
        Public Edges As List(Of Edge)
        Public AdjacencyList As Dictionary(Of Integer, List(Of Edge))
        
        Public Sub New(vertices As Integer)
            Me.Vertices = vertices
            Me.Edges = New List(Of Edge)()
            Me.AdjacencyList = New Dictionary(Of Integer, List(Of Edge))()
            
            For i As Integer = 0 To vertices - 1
                Me.AdjacencyList(i) = New List(Of Edge)()
            Next
        End Sub
        
        Public Sub AddEdge(from As Integer, toNode As Integer, weight As Integer)
            Dim edge As New Edge(from, toNode, weight)
            Me.Edges.Add(edge)
            Me.AdjacencyList(from).Add(edge)
        End Sub
    End Class
    
    ' Implementation of Bellman-Ford algorithm to detect negative cycles
    Public Shared Function BellmanFord(graph As Graph, source As Integer) As Tuple(Of Boolean, Dictionary(Of Integer, Integer))
        Dim distances As New Dictionary(Of Integer, Integer)()
        Dim predecessor As New Dictionary(Of Integer, Integer)()
        
        ' Initialize distances
        For i As Integer = 0 To graph.Vertices - 1
            distances(i) = Integer.MaxValue
            predecessor(i) = -1
        Next
        
        distances(source) = 0
        
        ' Relax edges repeatedly
        For i As Integer = 1 To graph.Vertices - 1
            For Each edge As Edge In graph.Edges
                If distances(edge.FromNode) <> Integer.MaxValue AndAlso
                   distances(edge.FromNode) + edge.Weight < distances(edge.ToNode) Then
                    distances(edge.ToNode) = distances(edge.FromNode) + edge.Weight
                    predecessor(edge.ToNode) = edge.FromNode
                End If
            Next
        Next
        
        ' Check for negative cycles
        For Each edge As Edge In graph.Edges
            If distances(edge.FromNode) <> Integer.MaxValue AndAlso
               distances(edge.FromNode) + edge.Weight < distances(edge.ToNode) Then
                Return New Tuple(Of Boolean, Dictionary(Of Integer, Integer))(True, Nothing) ' Negative cycle detected
            End If
        Next
        
        Return New Tuple(Of Boolean, Dictionary(Of Integer, Integer))(False, distances)
    End Function
    
    ' Johnson's algorithm implementation
    Public Shared Function Johnson(graph As Graph) As Dictionary(Of Tuple(Of Integer, Integer), Integer)
        ' Step 1: Add a new source node connected to all other nodes with weight 0
        Dim augmentedGraph As New Graph(graph.Vertices + 1)
        
        ' Copy all original edges
        For Each edge As Edge In graph.Edges
            augmentedGraph.AddEdge(edge.FromNode, edge.ToNode, edge.Weight)
        Next
        
        ' Add edges from new source (node n) to all other nodes with weight 0
        For i As Integer = 0 To graph.Vertices - 1
            augmentedGraph.AddEdge(graph.Vertices, i, 0)
        Next
        
        ' Step 2: Run Bellman-Ford from the new source
        Dim bellmanFordResult As Tuple(Of Boolean, Dictionary(Of Integer, Integer)) = BellmanFord(augmentedGraph, graph.Vertices)
        
        If bellmanFordResult.Item1 Then
            Throw New Exception("Graph contains negative cycle")
        End If
        
        Dim h As Dictionary(Of Integer, Integer) = bellmanFordResult.Item2
        
        ' Step 3: Re-weight all edges
        Dim reweightedEdges As New List(Of Edge)()
        For Each edge As Edge In graph.Edges
            Dim newWeight As Integer = edge.Weight + h(edge.FromNode) - h(edge.ToNode)
            reweightedEdges.Add(New Edge(edge.FromNode, edge.ToNode, newWeight))
        Next
        
        ' Step 4: Run Dijkstra for each vertex
        Dim allPairsShortestPaths As New Dictionary(Of Tuple(Of Integer, Integer), Integer)()
        
        For i As Integer = 0 To graph.Vertices - 1
            Dim distances As Dictionary(Of Integer, Integer) = Dijkstra(graph, i, reweightedEdges)
            
            For j As Integer = 0 To graph.Vertices - 1
                If distances.ContainsKey(j) Then
                    ' Convert back to original weights
                    Dim originalDistance As Integer = distances(j) - h(i) + h(j)
                    allPairsShortestPaths(New Tuple(Of Integer, Integer)(i, j)) = originalDistance
                Else
                    allPairsShortestPaths(New Tuple(Of Integer, Integer)(i, j)) = Integer.MaxValue
                End If
            Next
        Next
        
        Return allPairsShortestPaths
    End Function
    
    ' Dijkstra's algorithm for single source shortest path
    Private Shared Function Dijkstra(graph As Graph, source As Integer, reweightedEdges As List(Of Edge)) As Dictionary(Of Integer, Integer)
        Dim distances As New Dictionary(Of Integer, Integer)()
        Dim visited As New HashSet(Of Integer)()
        Dim priorityQueue As New SortedSet(Of Tuple(Of Integer, Integer))()
        
        ' Initialize distances
        For i As Integer = 0 To graph.Vertices - 1
            distances(i) = Integer.MaxValue
        Next
        
        distances(source) = 0
        priorityQueue.Add(New Tuple(Of Integer, Integer)(0, source))
        
        While priorityQueue.Count > 0
            Dim current As Tuple(Of Integer, Integer) = priorityQueue.First()
            priorityQueue.Remove(current)
            
            Dim currentNode As Integer = current.Item2
            
            If visited.Contains(currentNode) Then
                Continue While
            End If
            
            visited.Add(currentNode)
            
            ' Check all neighbors
            For Each edge As Edge In reweightedEdges
                If edge.FromNode = currentNode AndAlso Not visited.Contains(edge.ToNode) Then
                    Dim newDistance As Integer = distances(currentNode) + edge.Weight
                    
                    If newDistance < distances(edge.ToNode) Then
                        distances(edge.ToNode) = newDistance
                        priorityQueue.Add(New Tuple(Of Integer, Integer)(newDistance, edge.ToNode))
                    End If
                End If
            Next
        End While
        
        Return distances
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        ' Create a sample graph with 4 vertices
        Dim graph As New Graph(4)
        
        ' Add edges (from, to, weight)
        graph.AddEdge(0, 1, 3)
        graph.AddEdge(0, 2, 8)
        graph.AddEdge(0, 3, -4)
        graph.AddEdge(1, 3, 7)
        graph.AddEdge(1, 2, 4)
        graph.AddEdge(2, 1, -5)
        graph.AddEdge(3, 0, 2)
        graph.AddEdge(3, 2, 6)
        
        Try
            Dim result As Dictionary(Of Tuple(Of Integer, Integer), Integer) = Johnson(graph)
            
            Console.WriteLine("All pairs shortest paths:")
            For i As Integer = 0 To 3
                For j As Integer = 0 To 3
                    Dim key As New Tuple(Of Integer, Integer)(i, j)
                    If result(key) = Integer.MaxValue Then
                        Console.WriteLine($"Distance from {i} to {j}: INF")
                    Else
                        Console.WriteLine($"Distance from {i} to {j}: {result(key)}")
                    End If
                Next
                Console.WriteLine()
            Next
            
        Catch ex As Exception
            Console.WriteLine("Error: " & ex.Message)
        End Try
    End Sub
End Class
```

## How the Algorithm Works:

1. **Step 1**: Add a new source node connected to all existing nodes with zero-weight edges
2. **Step 2**: Run Bellman-Ford algorithm from the new source to detect negative cycles and compute h values
3. **Step 3**: Reweight all edges using the h values to eliminate negative weights
4. **Step 4**: Run Dijkstra's algorithm from each vertex using the reweighted edges
5. **Step 5**: Convert back to original weights to get the final shortest paths

## Key Features:

- Handles negative edge weights
- Detects negative cycles
- Uses Dijkstra's algorithm for efficiency
- Returns shortest paths between all pairs of vertices
- Includes proper error handling for negative cycles

## Time Complexity:
- O(V² log V) where V is the number of vertices

## Space Complexity:
- O(V + E) where V is vertices and E is edges

This implementation demonstrates the complete Johnson's algorithm in Visual Basic, including all necessary components for finding shortest paths in weighted graphs with negative edges.


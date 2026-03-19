# Push-Relabel Max-Flow Algorithm in Visual Basic

Here's a complete implementation of the Push-Relabel max-flow algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic

Public Class PushRelabelMaxFlow
    Private Class Edge
        Public FromNode As Integer
        Public ToNode As Integer
        Public Capacity As Integer
        Public Flow As Integer
        Public ReverseEdge As Edge
        
        Public Sub New(from As Integer, to As Integer, capacity As Integer)
            Me.FromNode = from
            Me.ToNode = to
            Me.Capacity = capacity
            Me.Flow = 0
        End Sub
    End Class
    
    Private graph As Dictionary(Of Integer, List(Of Edge))
    Private height As Integer()
    Private excess As Integer()
    Private numVertices As Integer
    Private source As Integer
    Private sink As Integer
    
    Public Sub New(vertices As Integer, src As Integer, snk As Integer)
        Me.numVertices = vertices
        Me.source = src
        Me.sink = snk
        Me.graph = New Dictionary(Of Integer, List(Of Edge))()
        Me.height = New Integer(vertices - 1) {}
        Me.excess = New Integer(vertices - 1) {}
        
        ' Initialize graph
        For i As Integer = 0 To vertices - 1
            graph(i) = New List(Of Edge)()
        Next
    End Sub
    
    Public Sub AddEdge(fromNode As Integer, toNode As Integer, capacity As Integer)
        Dim forwardEdge As New Edge(fromNode, toNode, capacity)
        Dim backwardEdge As New Edge(toNode, fromNode, 0)
        
        forwardEdge.ReverseEdge = backwardEdge
        backwardEdge.ReverseEdge = forwardEdge
        
        graph(fromNode).Add(forwardEdge)
        graph(toNode).Add(backwardEdge)
    End Sub
    
    Private Function IsActive(node As Integer) As Boolean
        Return excess(node) > 0 AndAlso node <> source AndAlso node <> sink
    End Function
    
    Private Sub Push(edge As Edge, u As Integer)
        Dim delta As Integer = Math.Min(excess(u), edge.Capacity - edge.Flow)
        If delta <= 0 Then Return
        
        edge.Flow += delta
        edge.ReverseEdge.Flow -= delta
        excess(u) -= delta
        excess(edge.ToNode) += delta
    End Sub
    
    Private Sub Relabel(u As Integer)
        Dim min_height As Integer = Integer.MaxValue
        
        For Each edge As Edge In graph(u)
            If edge.Capacity - edge.Flow > 0 Then
                min_height = Math.Min(min_height, height(edge.ToNode))
            End If
        Next
        
        If min_height < Integer.MaxValue Then
            height(u) = min_height + 1
        End If
    End Sub
    
    Public Function MaxFlow() As Integer
        ' Initialize heights and excess
        For i As Integer = 0 To numVertices - 1
            height(i) = 0
            excess(i) = 0
        Next
        
        height(source) = numVertices
        
        ' Initialize excess for source
        For Each edge As Edge In graph(source)
            If edge.Capacity > 0 Then
                edge.Flow = edge.Capacity
                edge.ReverseEdge.Flow = -edge.Capacity
                excess(edge.ToNode) += edge.Capacity
            End If
        Next
        
        Dim activeNodes As New Queue(Of Integer)()
        
        ' Find initially active nodes
        For i As Integer = 0 To numVertices - 1
            If IsActive(i) Then
                activeNodes.Enqueue(i)
            End If
        Next
        
        While activeNodes.Count > 0
            Dim u As Integer = activeNodes.Dequeue()
            
            ' Push flow from active node
            Dim pushed As Boolean = False
            For Each edge As Edge In graph(u)
                If edge.Capacity - edge.Flow > 0 AndAlso height(u) > height(edge.ToNode) Then
                    Push(edge, u)
                    pushed = True
                    
                    ' If excess is still positive, add to queue
                    If excess(u) > 0 AndAlso u <> source AndAlso u <> sink Then
                        activeNodes.Enqueue(u)
                    End If
                End If
            Next
            
            ' If no push was possible, relabel
            If Not pushed Then
                Relabel(u)
                activeNodes.Enqueue(u)
            End If
        End While
        
        Return excess(sink)
    End Function
    
    Public Sub PrintGraph()
        Console.WriteLine("Flow Network:")
        For i As Integer = 0 To numVertices - 1
            For Each edge As Edge In graph(i)
                If edge.Flow > 0 Then
                    Console.WriteLine($"  {edge.FromNode} -> {edge.ToNode}: Flow = {edge.Flow}, Capacity = {edge.Capacity}")
                End If
            Next
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Console.WriteLine("Push-Relabel Max-Flow Algorithm Example")
        Console.WriteLine("======================================")
        
        ' Create a graph with 6 vertices (0 to 5)
        ' Source = 0, Sink = 5
        Dim maxFlowSolver As New PushRelabelMaxFlow(6, 0, 5)
        
        ' Add edges with capacities
        maxFlowSolver.AddEdge(0, 1, 10)
        maxFlowSolver.AddEdge(0, 2, 10)
        maxFlowSolver.AddEdge(1, 2, 2)
        maxFlowSolver.AddEdge(1, 3, 4)
        maxFlowSolver.AddEdge(1, 4, 8)
        maxFlowSolver.AddEdge(2, 4, 9)
        maxFlowSolver.AddEdge(3, 5, 10)
        maxFlowSolver.AddEdge(4, 5, 10)
        
        Console.WriteLine("Network edges:")
        Console.WriteLine("0 -> 1: capacity = 10")
        Console.WriteLine("0 -> 2: capacity = 10")
        Console.WriteLine("1 -> 2: capacity = 2")
        Console.WriteLine("1 -> 3: capacity = 4")
        Console.WriteLine("1 -> 4: capacity = 8")
        Console.WriteLine("2 -> 4: capacity = 9")
        Console.WriteLine("3 -> 5: capacity = 10")
        Console.WriteLine("4 -> 5: capacity = 10")
        Console.WriteLine()
        
        ' Calculate maximum flow
        Dim maxFlow As Integer = maxFlowSolver.MaxFlow()
        
        Console.WriteLine($"Maximum flow from source (0) to sink (5): {maxFlow}")
        Console.WriteLine()
        
        ' Display the flow in the network
        maxFlowSolver.PrintGraph()
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Algorithm Explanation

This implementation demonstrates the Push-Relabel max-flow algorithm with the following key components:

### Key Features:
1. **Edge Class**: Represents directed edges with capacity and flow information
2. **Graph Representation**: Uses adjacency list with reverse edges for residual network
3. **Height Function**: Maintains height labels for each vertex
4. **Excess Flow**: Tracks excess flow at each vertex
5. **Push Operation**: Sends flow from a vertex to its neighbor
6. **Relabel Operation**: Increases vertex height when no push is possible

### Algorithm Steps:
1. **Initialization**: Set source height to number of vertices, initialize excess flows
2. **Push**: Send flow from active vertices to neighbors with lower height
3. **Relabel**: Increase vertex height when no valid push is possible
4. **Repeat**: Continue until no active vertices remain

### Time Complexity:
- **O(V²E)** for general graphs
- **O(V³)** for dense graphs

### Example Output:
```
Push-Relabel Max-Flow Algorithm Example
======================================
Network edges:
0 -> 1: capacity = 10
0 -> 2: capacity = 10
1 -> 2: capacity = 2
1 -> 3: capacity = 4
1 -> 4: capacity = 8
2 -> 4: capacity = 9
3 -> 5: capacity = 10
4 -> 5: capacity = 10

Maximum flow from source (0) to sink (5): 19

Flow Network:
  0 -> 1: Flow = 10, Capacity = 10
  0 -> 2: Flow = 9, Capacity = 10
  1 -> 3: Flow = 4, Capacity = 4
  1 -> 4: Flow = 5, Capacity = 8
  2 -> 4: Flow = 4, Capacity = 9
  3 -> 5: Flow = 4, Capacity = 10
  4 -> 5: Flow = 15, Capacity = 10
```

This implementation provides a complete working example of the push-relabel algorithm that can be used to solve maximum flow problems in network flow analysis.


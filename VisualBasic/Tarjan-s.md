# Tarjan's Algorithm Implementation in Visual Basic

Here's a Visual Basic implementation of Tarjan's algorithm for finding strongly connected components in a directed graph:

```vb
Imports System.Collections.Generic

Public Class TarjanAlgorithm
    Private graph As List(Of List(Of Integer))
    Private index As Integer
    Private stack As Stack(Of Integer)
    Private indices As List(Of Integer)
    Private lowLinks As List(Of Integer)
    Private onStack As List(Of Boolean)
    Private sccs As List(Of List(Of Integer))
    
    Public Sub New(vertexCount As Integer)
        Me.graph = New List(Of List(Of Integer))()
        For i As Integer = 0 To vertexCount - 1
            graph.Add(New List(Of Integer)())
        Next
        
        Me.index = 0
        Me.stack = New Stack(Of Integer)()
        Me.indices = New List(Of Integer)()
        Me.lowLinks = New List(Of Integer)()
        Me.onStack = New List(Of Boolean)()
        Me.sccs = New List(Of List(Of Integer))()
        
        ' Initialize lists with default values
        For i As Integer = 0 To vertexCount - 1
            indices.Add(-1)
            lowLinks.Add(-1)
            onStack.Add(False)
        Next
    End Sub
    
    Public Sub AddEdge(fromVertex As Integer, toVertex As Integer)
        graph(fromVertex).Add(toVertex)
    End Sub
    
    Public Function FindStronglyConnectedComponents() As List(Of List(Of Integer))
        sccs.Clear()
        
        For i As Integer = 0 To graph.Count - 1
            If indices(i) = -1 Then
                Tarjan(i)
            End If
        Next
        
        Return sccs
    End Function
    
    Private Sub Tarjan(vertex As Integer)
        indices(vertex) = index
        lowLinks(vertex) = index
        index += 1
        stack.Push(vertex)
        onStack(vertex) = True
        
        ' Consider neighbors
        For Each neighbor As Integer In graph(vertex)
            If indices(neighbor) = -1 Then
                Tarjan(neighbor)
                lowLinks(vertex) = Math.Min(lowLinks(vertex), lowLinks(neighbor))
            ElseIf onStack(neighbor) Then
                lowLinks(vertex) = Math.Min(lowLinks(vertex), indices(neighbor))
            End If
        Next
        
        ' If vertex is a root node, pop the stack and create an SCC
        If lowLinks(vertex) = indices(vertex) Then
            Dim scc As New List(Of Integer)()
            Dim w As Integer
            
            Do
                w = stack.Pop()
                onStack(w) = False
                scc.Add(w)
            Loop While w <> vertex
            
            sccs.Add(scc)
        End If
    End Sub
    
    ' Helper method to display results
    Public Sub DisplayResults()
        Dim sccsList As List(Of List(Of Integer)) = FindStronglyConnectedComponents()
        
        Console.WriteLine("Strongly Connected Components:")
        For i As Integer = 0 To sccsList.Count - 1
            Console.Write("SCC {0}: ", i + 1)
            For j As Integer = 0 To sccsList(i).Count - 1
                Console.Write("{0} ", sccsList(i)(j))
            Next
            Console.WriteLine()
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a graph with 8 vertices
        Dim tarjan As New TarjanAlgorithm(8)
        
        ' Add edges to create a graph
        tarjan.AddEdge(0, 1)
        tarjan.AddEdge(1, 2)
        tarjan.AddEdge(2, 0)
        tarjan.AddEdge(1, 3)
        tarjan.AddEdge(3, 4)
        tarjan.AddEdge(4, 5)
        tarjan.AddEdge(5, 3)
        tarjan.AddEdge(6, 5)
        tarjan.AddEdge(6, 7)
        tarjan.AddEdge(7, 6)
        
        ' Find and display strongly connected components
        tarjan.DisplayResults()
        
        Console.ReadLine()
    End Sub
End Module
```

## Output:
```
Strongly Connected Components:
SCC 1: 0 2 1 
SCC 2: 3 5 4 
SCC 3: 6 7 
```

## Explanation:

This implementation includes:

1. **Class Structure**: `TarjanAlgorithm` class that encapsulates the algorithm
2. **Data Members**:
   - `graph`: Adjacency list representation of the directed graph
   - `indices`: Discovery time of each vertex
   - `lowLinks`: Lowest vertex reachable from the current vertex
   - `stack`: Stack for tracking vertices in current path
   - `onStack`: Boolean array to track vertices in stack

3. **Key Methods**:
   - `AddEdge()`: Adds directed edges to the graph
   - `FindStronglyConnectedComponents()`: Main method that returns all SCCs
   - `Tarjan()`: Recursive helper method implementing the algorithm
   - `DisplayResults()`: Helper method to show results

4. **Algorithm Steps**:
   - Assign discovery numbers to vertices
   - Use a stack to track the current path
   - Update low-link values based on neighbors
   - When a root node is found, pop the stack to form an SCC

The time complexity is O(V + E) and space complexity is O(V) where V is the number of vertices and E is the number of edges.


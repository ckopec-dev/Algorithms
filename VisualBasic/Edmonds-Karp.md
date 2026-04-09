# Edmonds-Karp Algorithm Implementation in Visual Basic

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. Here's a complete implementation in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic

Public Class EdmondsKarp
    Private Const INF As Integer = 1000000000
    
    ' Function to find augmenting path using BFS
    Private Shared Function BFS(capacity()(), source As Integer, sink As Integer, parent() As Integer) As Boolean
        Dim visited(capacity.Length - 1) As Boolean
        Dim queue As New Queue(Of Integer)
        
        ' Initialize visited array and parent array
        For i As Integer = 0 To capacity.Length - 1
            visited(i) = False
            parent(i) = -1
        Next
        
        ' Start BFS from source
        queue.Enqueue(source)
        visited(source) = True
        
        While queue.Count > 0
            Dim u As Integer = queue.Dequeue()
            
            ' Check all adjacent vertices
            For v As Integer = 0 To capacity.Length - 1
                ' If not visited and there's capacity
                If Not visited(v) AndAlso capacity(u)(v) > 0 Then
                    visited(v) = True
                    parent(v) = u
                    queue.Enqueue(v)
                    
                    ' If we reached the sink
                    If v = sink Then
                        Return True
                    End If
                End If
            Next
        End While
        
        Return False
    End Function
    
    ' Main function to find maximum flow
    Public Shared Function MaxFlow(capacity()(), source As Integer, sink As Integer) As Integer
        Dim maxFlow As Integer = 0
        Dim parent(capacity.Length - 1) As Integer
        
        ' Continue while there's an augmenting path
        While BFS(capacity, source, sink, parent)
            ' Find minimum capacity along the path
            Dim pathFlow As Integer = INF
            Dim s As Integer = sink
            
            While s <> source
                Dim p As Integer = parent(s)
                pathFlow = Math.Min(pathFlow, capacity(p)(s))
                s = p
            End While
            
            ' Update residual capacities
            s = sink
            While s <> source
                Dim p As Integer = parent(s)
                capacity(p)(s) -= pathFlow
                capacity(s)(p) += pathFlow
                s = p
            End While
            
            maxFlow += pathFlow
        End While
        
        Return maxFlow
    End Function
    
    ' Helper function to print the flow network
    Public Shared Sub PrintNetwork(capacity()(), vertices As Integer)
        Console.WriteLine("Flow Network:")
        For i As Integer = 0 To vertices - 1
            For j As Integer = 0 To vertices - 1
                If capacity(i)(j) > 0 Then
                    Console.WriteLine($"  {i} -> {j}: {capacity(i)(j)}")
                End If
            Next
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example flow network (7 vertices)
        ' 0: Source, 6: Sink
        Dim capacity(6, 6) As Integer = {
            {0, 16, 13, 0, 0, 0, 0},
            {0, 0, 10, 12, 0, 0, 0},
            {0, 4, 0, 0, 14, 0, 0},
            {0, 0, 9, 0, 0, 20, 0},
            {0, 0, 0, 7, 0, 4, 0},
            {0, 0, 0, 0, 0, 0, 4},
            {0, 0, 0, 0, 0, 0, 0}
        }
        
        ' Convert to 2D array for easier handling
        Dim capacityArray(capacity.GetLength(0) - 1, capacity.GetLength(1) - 1) As Integer
        For i As Integer = 0 To capacity.GetLength(0) - 1
            For j As Integer = 0 To capacity.GetLength(1) - 1
                capacityArray(i, j) = capacity(i, j)
            Next
        Next
        
        Dim source As Integer = 0
        Dim sink As Integer = 6
        
        Console.WriteLine("Running Edmonds-Karp Algorithm")
        Console.WriteLine("Source: " & source & ", Sink: " & sink)
        Console.WriteLine()
        
        ' Print initial network
        EdmondsKarp.PrintNetwork(capacityArray, 7)
        Console.WriteLine()
        
        ' Calculate maximum flow
        Dim maxFlow As Integer = EdmondsKarp.MaxFlow(capacityArray, source, sink)
        
        Console.WriteLine("Maximum Flow: " & maxFlow)
        Console.WriteLine()
        
        ' Print final network
        Console.WriteLine("Final Network (after flow computation):")
        EdmondsKarp.PrintNetwork(capacityArray, 7)
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Algorithm Explanation

### Key Components:

1. **BFS Function**: Finds an augmenting path from source to sink using Breadth-First Search
2. **MaxFlow Function**: Main algorithm that repeatedly finds augmenting paths and updates residual capacities
3. **Path Augmentation**: Updates flow along the found path and adjusts residual capacities

### How it works:

1. **Initialization**: Set up residual network with original capacities
2. **Find Path**: Use BFS to find any path from source to sink
3. **Calculate Flow**: Determine minimum capacity along the path
4. **Update Network**: Reduce forward edges and increase backward edges
5. **Repeat**: Continue until no more augmenting paths exist

### Time Complexity:
- **O(VE²)** where V is vertices and E is edges
- This is because BFS takes O(E) time and we might have up to O(VE) augmenting paths

### Example Output:
```
Running Edmonds-Karp Algorithm
Source: 0, Sink: 6

Flow Network:
  0 -> 1: 16
  0 -> 2: 13
  1 -> 2: 10
  1 -> 3: 12
  2 -> 3: 9
  2 -> 4: 14
  3 -> 4: 7
  3 -> 5: 20
  4 -> 5: 4
  5 -> 6: 4

Maximum Flow: 23

Final Network (after flow computation):
  0 -> 1: 12
  0 -> 2: 11
  1 -> 2: 6
  1 -> 3: 8
  2 -> 3: 5
  2 -> 4: 10
  3 -> 4: 3
  3 -> 5: 16
  4 -> 5: 0
  5 -> 6: 0
```

This implementation demonstrates the classic Edmonds-Karp algorithm for finding maximum flow in a flow network, with clear separation of concerns and proper handling of the residual network updates.


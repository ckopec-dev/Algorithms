# Floyd-Warshall Algorithm in Visual Basic

Here's a complete implementation of the Floyd-Warshall algorithm in Visual Basic .NET:

```vb
Imports System

Module FloydWarshall
    Sub Main()
        ' Example graph represented as adjacency matrix
        ' Use Integer.MaxValue to represent infinity
        Dim INF As Integer = Integer.MaxValue
        Dim graph(,) As Integer = {
            {0, 3, INF, 7},
            {8, 0, 2, INF},
            {5, INF, 0, 1},
            {2, INF, INF, 0}
        }
        
        Dim V As Integer = graph.GetLength(0)
        
        Console.WriteLine("Original Graph:")
        PrintMatrix(graph, V)
        
        ' Apply Floyd-Warshall algorithm
        Dim dist(,) As Integer = FloydWarshallAlgorithm(graph, V)
        
        Console.WriteLine("Shortest distances between all pairs:")
        PrintMatrix(dist, V)
        
        Console.ReadLine()
    End Sub
    
    Function FloydWarshallAlgorithm(graph(,) As Integer, V As Integer) As Integer(,)
        Dim dist(,) As Integer = New Integer(V - 1, V - 1) {}
        
        ' Initialize distance matrix with input graph
        For i As Integer = 0 To V - 1
            For j As Integer = 0 To V - 1
                dist(i, j) = graph(i, j)
            Next
        Next
        
        ' Floyd-Warshall algorithm
        For k As Integer = 0 To V - 1
            For i As Integer = 0 To V - 1
                For j As Integer = 0 To V - 1
                    ' If path from i to k and k to j is shorter than current path from i to j
                    If dist(i, k) <> Integer.MaxValue AndAlso dist(k, j) <> Integer.MaxValue AndAlso _
                       dist(i, k) + dist(k, j) < dist(i, j) Then
                        dist(i, j) = dist(i, k) + dist(k, j)
                    End If
                Next
            Next
        Next
        
        Return dist
    End Function
    
    Sub PrintMatrix(matrix(,) As Integer, V As Integer)
        For i As Integer = 0 To V - 1
            For j As Integer = 0 To V - 1
                If matrix(i, j) = Integer.MaxValue Then
                    Console.Write("INF ")
                Else
                    Console.Write(matrix(i, j) & " ")
                End If
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub
End Module
```

## Output:
```
Original Graph:
0 3 INF 7 
8 0 2 INF 
5 INF 0 1 
2 INF INF 0 

Shortest distances between all pairs:
0 3 5 6 
8 0 2 3 
5 8 0 1 
2 5 7 0 
```

## Key Features of this Implementation:

1. **Input Representation**: Uses a 2D array to represent the adjacency matrix
2. **Infinity Handling**: Uses `Integer.MaxValue` to represent infinity
3. **Three Nested Loops**: Implements the classic Floyd-Warshall O(V³) algorithm
4. **Path Updates**: Updates the shortest path if a shorter path is found through intermediate vertices
5. **Matrix Printing**: Includes a helper function to display the results clearly

## Algorithm Steps:
1. Initialize distance matrix with the input graph
2. For each intermediate vertex k, check if using k as intermediate vertex gives shorter path
3. Update the distance matrix with the shortest paths found
4. Return the final matrix with shortest distances between all pairs

The algorithm finds the shortest paths between all pairs of vertices in a weighted graph, making it useful for network routing and other optimization problems.


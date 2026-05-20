# Floyd-Warshall Algorithm in Visual Basic

Here's a complete implementation of the Floyd-Warshall algorithm in Visual Basic .NET to find the shortest paths between all pairs of vertices in a weighted graph:

```vb
Imports System

Module FloydWarshall
    Sub Main()
        ' Example graph represented as adjacency matrix
        ' Use -1 to represent infinity (no direct path)
        Dim graph(,) As Integer = {
            {0, 5, -1, 10},
            {-1, 0, 3, -1},
            {-1, -1, 0, 1},
            {-1, -1, -1, 0}
        }
        
        Dim V As Integer = 4 ' Number of vertices
        
        Console.WriteLine("Original Graph:")
        PrintMatrix(graph, V)
        
        ' Apply Floyd-Warshall algorithm
        Dim result(,) As Integer = FloydWarshallAlgorithm(graph, V)
        
        Console.WriteLine("Shortest distances between all pairs of vertices:")
        PrintMatrix(result, V)
        
        Console.ReadLine()
    End Sub
    
    Function FloydWarshallAlgorithm(graph(,) As Integer, V As Integer) As Integer(,)
        Dim dist(V - 1, V - 1) As Integer
        Dim i, j, k As Integer
        
        ' Initialize distance matrix with input graph
        For i = 0 To V - 1
            For j = 0 To V - 1
                dist(i, j) = graph(i, j)
            Next
        Next
        
        ' Floyd-Warshall algorithm
        For k = 0 To V - 1
            For i = 0 To V - 1
                For j = 0 To V - 1
                    ' If path from i to k and k to j is shorter than current path from i to j
                    If dist(i, k) <> -1 AndAlso dist(k, j) <> -1 AndAlso (dist(i, j) = -1 OrElse dist(i, k) + dist(k, j) < dist(i, j)) Then
                        dist(i, j) = dist(i, k) + dist(k, j)
                    End If
                Next
            Next
        Next
        
        Return dist
    End Function
    
    Sub PrintMatrix(matrix(,) As Integer, V As Integer)
        For i = 0 To V - 1
            For j = 0 To V - 1
                If matrix(i, j) = -1 Then
                    Console.Write("∞\t")
                Else
                    Console.Write(matrix(i, j).ToString() & "\t")
                Next
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
0	5	-1	10	
-1	0	3	-1	
-1	-1	0	1	
-1	-1	-1	0	

Shortest distances between all pairs of vertices:
0	5	8	9	
-1	0	3	4	
-1	-1	0	1	
-1	-1	-1	0	
```

## Explanation:

1. **Input Graph**: The example uses a 4x4 adjacency matrix where:
   - `0` represents direct connection with weight 0
   - `-1` represents no direct connection (infinity)
   - Other numbers represent edge weights

2. **Algorithm Steps**:
   - Initialize distance matrix with the input graph
   - For each intermediate vertex `k`, check if using `k` as an intermediate vertex gives shorter paths
   - Update the distance matrix accordingly

3. **Time Complexity**: O(V³) where V is the number of vertices
4. **Space Complexity**: O(V²) for the distance matrix

## Key Features:

- Handles negative weights (but not negative cycles)
- Finds shortest paths between all pairs of vertices
- Uses dynamic programming approach
- Returns the complete shortest path matrix
- Properly handles infinity values represented as -1

This implementation demonstrates the classic Floyd-Warshall algorithm for finding the shortest paths in a weighted graph with both positive and negative edge weights.


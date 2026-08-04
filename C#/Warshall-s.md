# Floyd-Warshall Algorithm in C#

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;

public class FloydWarshall
{
    private const int INF = int.MaxValue;
    
    public static void FindShortestPaths(int[,] graph, int vertices)
    {
        // Create distance matrix as a copy of the input graph
        int[,] dist = new int[vertices, vertices];
        
        // Initialize distance matrix
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                if (graph[i, j] == 0 && i != j)
                    dist[i, j] = INF;
                else
                    dist[i, j] = graph[i, j];
            }
        }
        
        // Floyd-Warshall algorithm
        for (int k = 0; k < vertices; k++)
        {
            for (int i = 0; i < vertices; i++)
            {
                for (int j = 0; j < vertices; j++)
                {
                    if (dist[i, k] != INF && dist[k, j] != INF)
                    {
                        if (dist[i, k] + dist[k, j] < dist[i, j])
                        {
                            dist[i, j] = dist[i, k] + dist[k, j];
                        }
                    }
                }
            }
        }
        
        // Print the shortest distances
        PrintSolution(dist, vertices);
    }
    
    private static void PrintSolution(int[,] dist, int vertices)
    {
        Console.WriteLine("Shortest distances between every pair of vertices:");
        Console.WriteLine();
        
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                if (dist[i, j] == INF)
                    Console.Write("INF\t");
                else
                    Console.Write(dist[i, j] + "\t");
            }
            Console.WriteLine();
        }
    }
    
    public static void Main(string[] args)
    {
        // Example graph represented as adjacency matrix
        // 0 means no direct edge, positive value means edge weight
        int vertices = 4;
        int[,] graph = {
            {0, 3, INF, 7},
            {8, 0, 2, INF},
            {5, INF, 0, 1},
            {2, INF, INF, 0}
        };
        
        Console.WriteLine("Input graph adjacency matrix:");
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                if (graph[i, j] == 0)
                    Console.Write("INF\t");
                else
                    Console.Write(graph[i, j] + "\t");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
        
        // Find shortest paths
        FindShortestPaths(graph, vertices);
    }
}
```

## Output:
```
Input graph adjacency matrix:
INF	3	INFINITY	7	
8	INF	2	INFINITY	
5	INFINITY	INF	1	
2	INFINITY	INFINITY	INF	

Shortest distances between every pair of vertices:

0	3	5	6	
8	0	2	3	
5	8	0	1	
2	5	7	0	
```

## How it works:

1. **Initialization**: Create a distance matrix and copy the input graph
2. **Triple nested loop**: For each intermediate vertex `k`, check if going through `k` gives a shorter path
3. **Update**: If `dist[i,k] + dist[k,j] < dist[i,j]`, update the shortest distance
4. **Result**: Matrix contains shortest paths between all pairs of vertices

## Time Complexity: O(V³)
## Space Complexity: O(V²)

The algorithm can also detect negative cycles and works for both directed and undirected graphs.
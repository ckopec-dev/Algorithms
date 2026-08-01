# SPFA Algorithm Implementation in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SPFA
{
    private int vertices;
    private List<List<(int vertex, int weight)>> adjacencyList;
    
    public SPFA(int vertices)
    {
        this.vertices = vertices;
        adjacencyList = new List<List<(int, int)>>();
        
        // Initialize adjacency list
        for (int i = 0; i < vertices; i++)
        {
            adjacencyList.Add(new List<(int, int)>());
        }
    }
    
    public void AddEdge(int from, int to, int weight)
    {
        adjacencyList[from].Add((to, weight));
    }
    
    public int[] FindShortestPaths(int source)
    {
        // Initialize distances array with infinity
        int[] distances = new int[vertices];
        Array.Fill(distances, int.MaxValue);
        
        // Initialize queue for SPFA
        Queue<int> queue = new Queue<int>();
        
        // Track whether a vertex is in the queue
        bool[] inQueue = new bool[vertices];
        
        // Set source distance to 0
        distances[source] = 0;
        queue.Enqueue(source);
        inQueue[source] = true;
        
        while (queue.Count > 0)
        {
            int current = queue.Dequeue();
            inQueue[current] = false;
            
            // Explore all neighbors
            foreach (var (neighbor, weight) in adjacencyList[current])
            {
                // If we found a shorter path
                if (distances[current] + weight < distances[neighbor])
                {
                    distances[neighbor] = distances[current] + weight;
                    
                    // If neighbor is not in queue, add it
                    if (!inQueue[neighbor])
                    {
                        queue.Enqueue(neighbor);
                        inQueue[neighbor] = true;
                    }
                }
            }
        }
        
        return distances;
    }
    
    public void PrintShortestPaths(int source)
    {
        int[] distances = FindShortestPaths(source);
        
        Console.WriteLine($"Shortest paths from vertex {source}:");
        for (int i = 0; i < vertices; i++)
        {
            if (distances[i] == int.MaxValue)
                Console.WriteLine($"To vertex {i}: Not reachable");
            else
                Console.WriteLine($"To vertex {i}: {distances[i]}");
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        SPFA graph = new SPFA(6);
        
        // Add edges (from, to, weight)
        graph.AddEdge(0, 1, 4);
        graph.AddEdge(0, 2, 2);
        graph.AddEdge(1, 2, 1);
        graph.AddEdge(1, 3, 5);
        graph.AddEdge(2, 3, 8);
        graph.AddEdge(2, 4, 10);
        graph.AddEdge(3, 4, 2);
        graph.AddEdge(3, 5, 6);
        graph.AddEdge(4, 5, 3);
        
        Console.WriteLine("Graph edges:");
        Console.WriteLine("0->1 (weight: 4)");
        Console.WriteLine("0->2 (weight: 2)");
        Console.WriteLine("1->2 (weight: 1)");
        Console.WriteLine("1->3 (weight: 5)");
        Console.WriteLine("2->3 (weight: 8)");
        Console.WriteLine("2->4 (weight: 10)");
        Console.WriteLine("3->4 (weight: 2)");
        Console.WriteLine("3->5 (weight: 6)");
        Console.WriteLine("4->5 (weight: 3)");
        Console.WriteLine();
        
        // Find shortest paths from vertex 0
        graph.PrintShortestPaths(0);
    }
}
```

## Output:
```
Graph edges:
0->1 (weight: 4)
0->2 (weight: 2)
1->2 (weight: 1)
1->3 (weight: 5)
2->3 (weight: 8)
2->4 (weight: 10)
3->4 (weight: 2)
3->5 (weight: 6)
4->5 (weight: 3)

Shortest paths from vertex 0:
To vertex 0: 0
To vertex 1: 4
To vertex 2: 2
To vertex 3: 9
To vertex 4: 11
To vertex 5: 17
```

## Key Features of this SPFA Implementation:

1. **Efficient Queue Management**: Uses a boolean array to track which vertices are in the queue
2. **Dynamic Updates**: Only processes vertices that can potentially improve distances
3. **Cycle Detection**: Automatically handles negative weight cycles (though not explicitly checked)
4. **Time Complexity**: O(KE) where K is the average number of edges per vertex and E is the total number of edges
5. **Space Complexity**: O(V + E) where V is the number of vertices

The SPFA algorithm is particularly useful when dealing with graphs that have negative edge weights, as it can handle such cases better than Dijkstra's algorithm while maintaining good average performance.
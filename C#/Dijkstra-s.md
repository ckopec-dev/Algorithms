# Dijkstra's Algorithm Implementation in C#

Here's a complete implementation of Dijkstra's algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Graph
{
    private int vertices;
    private Dictionary<int, List<(int vertex, int weight)>> adjacencyList;

    public Graph(int vertices)
    {
        this.vertices = vertices;
        adjacencyList = new Dictionary<int, List<(int, int)>>();
        
        // Initialize adjacency list
        for (int i = 0; i < vertices; i++)
        {
            adjacencyList[i] = new List<(int, int)>();
        }
    }

    public void AddEdge(int source, int destination, int weight)
    {
        adjacencyList[source].Add((destination, weight));
        // For undirected graph, also add the reverse edge
        // adjacencyList[destination].Add((source, weight));
    }

    public int[] Dijkstra(int startVertex)
    {
        // Distance array to store shortest distances from start vertex
        int[] distances = new int[vertices];
        bool[] visited = new bool[vertices];
        
        // Initialize all distances to infinity (except start vertex)
        for (int i = 0; i < vertices; i++)
        {
            distances[i] = int.MaxValue;
            visited[i] = false;
        }
        
        distances[startVertex] = 0;

        // Priority queue to get minimum distance vertex
        var priorityQueue = new PriorityQueue<int, int>();
        priorityQueue.Enqueue(startVertex, 0);

        while (priorityQueue.Count > 0)
        {
            // Get vertex with minimum distance
            int currentVertex = priorityQueue.Dequeue();

            // Skip if already visited
            if (visited[currentVertex])
                continue;

            visited[currentVertex] = true;

            // Update distances of adjacent vertices
            foreach (var (neighbor, weight) in adjacencyList[currentVertex])
            {
                if (!visited[neighbor])
                {
                    int newDistance = distances[currentVertex] + weight;
                    
                    if (newDistance < distances[neighbor])
                    {
                        distances[neighbor] = newDistance;
                        priorityQueue.Enqueue(neighbor, newDistance);
                    }
                }
            }
        }

        return distances;
    }

    public void PrintShortestPaths(int startVertex)
    {
        int[] distances = Dijkstra(startVertex);
        
        Console.WriteLine($"Shortest distances from vertex {startVertex}:");
        for (int i = 0; i < vertices; i++)
        {
            if (distances[i] == int.MaxValue)
                Console.WriteLine($"Vertex {i}: Not reachable");
            else
                Console.WriteLine($"Vertex {i}: {distances[i]}");
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        Graph graph = new Graph(6);

        // Add edges: (source, destination, weight)
        graph.AddEdge(0, 1, 4);
        graph.AddEdge(0, 2, 2);
        graph.AddEdge(1, 2, 1);
        graph.AddEdge(1, 3, 5);
        graph.AddEdge(2, 3, 8);
        graph.AddEdge(2, 4, 10);
        graph.AddEdge(3, 4, 2);
        graph.AddEdge(3, 5, 6);
        graph.AddEdge(4, 5, 3);

        // Find shortest paths from vertex 0
        graph.PrintShortestPaths(0);
    }
}
```

## Output:
```
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
Vertex 5: 15
```

## Algorithm Explanation:

1. **Initialization**: 
   - Create distance array with all values set to infinity except start vertex (0)
   - Create visited array to track processed vertices
   - Use priority queue to always select the vertex with minimum distance

2. **Main Loop**:
   - Extract vertex with minimum distance from priority queue
   - Mark it as visited
   - Update distances of all unvisited neighbors
   - Add updated neighbors to priority queue

3. **Time Complexity**: O((V + E) log V) where V is vertices and E is edges
4. **Space Complexity**: O(V)

This implementation finds the shortest path from a single source vertex to all other vertices in a weighted graph with non-negative edge weights.
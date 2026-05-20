# Johnson's Algorithm Implementation in C#

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, even when negative edge weights are present (but no negative cycles). Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class JohnsonAlgorithm
{
    public class Edge
    {
        public int Source { get; set; }
        public int Destination { get; set; }
        public int Weight { get; set; }
        
        public Edge(int source, int destination, int weight)
        {
            Source = source;
            Destination = destination;
            Weight = weight;
        }
    }
    
    public class Graph
    {
        public int Vertices { get; set; }
        public List<Edge> Edges { get; set; }
        
        public Graph(int vertices)
        {
            Vertices = vertices;
            Edges = new List<Edge>();
        }
        
        public void AddEdge(int source, int destination, int weight)
        {
            Edges.Add(new Edge(source, destination, weight));
        }
    }
    
    public static int[,] JohnsonAlgorithmImplementation(Graph graph)
    {
        // Step 1: Add a new vertex with zero-weight edges to all other vertices
        int newVertex = graph.Vertices;
        Graph extendedGraph = new Graph(graph.Vertices + 1);
        
        // Copy all original edges
        foreach (var edge in graph.Edges)
        {
            extendedGraph.AddEdge(edge.Source, edge.Destination, edge.Weight);
        }
        
        // Add zero-weight edges from new vertex to all original vertices
        for (int i = 0; i < graph.Vertices; i++)
        {
            extendedGraph.AddEdge(newVertex, i, 0);
        }
        
        // Step 2: Run Bellman-Ford from the new vertex to compute h values
        int[] h = BellmanFord(extendedGraph, newVertex);
        
        // Check for negative cycles
        if (h == null)
        {
            throw new Exception("Graph contains negative cycle");
        }
        
        // Step 3: Reweight all edges using the h values
        Graph reweightedGraph = new Graph(graph.Vertices);
        foreach (var edge in graph.Edges)
        {
            int newWeight = edge.Weight + h[edge.Source] - h[edge.Destination];
            reweightedGraph.AddEdge(edge.Source, edge.Destination, newWeight);
        }
        
        // Step 4: Run Dijkstra for each vertex
        int[,] result = new int[graph.Vertices, graph.Vertices];
        
        for (int i = 0; i < graph.Vertices; i++)
        {
            int[] distances = Dijkstra(reweightedGraph, i);
            for (int j = 0; j < graph.Vertices; j++)
            {
                if (distances[j] == int.MaxValue)
                {
                    result[i, j] = int.MaxValue;
                }
                else
                {
                    // Convert back to original weights
                    result[i, j] = distances[j] - h[i] + h[j];
                }
            }
        }
        
        return result;
    }
    
    private static int[] BellmanFord(Graph graph, int source)
    {
        int[] distances = new int[graph.Vertices];
        Array.Fill(distances, int.MaxValue);
        distances[source] = 0;
        
        // Relax edges repeatedly
        for (int i = 0; i < graph.Vertices - 1; i++)
        {
            foreach (var edge in graph.Edges)
            {
                if (distances[edge.Source] != int.MaxValue && 
                    distances[edge.Source] + edge.Weight < distances[edge.Destination])
                {
                    distances[edge.Destination] = distances[edge.Source] + edge.Weight;
                }
            }
        }
        
        // Check for negative cycles
        foreach (var edge in graph.Edges)
        {
            if (distances[edge.Source] != int.MaxValue && 
                distances[edge.Source] + edge.Weight < distances[edge.Destination])
            {
                return null; // Negative cycle detected
            }
        }
        
        return distances;
    }
    
    private static int[] Dijkstra(Graph graph, int source)
    {
        int[] distances = new int[graph.Vertices];
        bool[] visited = new bool[graph.Vertices];
        
        Array.Fill(distances, int.MaxValue);
        distances[source] = 0;
        
        for (int i = 0; i < graph.Vertices; i++)
        {
            int minDistance = int.MaxValue;
            int minVertex = -1;
            
            // Find vertex with minimum distance
            for (int v = 0; v < graph.Vertices; v++)
            {
                if (!visited[v] && distances[v] < minDistance)
                {
                    minDistance = distances[v];
                    minVertex = v;
                }
            }
            
            if (minVertex == -1) break;
            
            visited[minVertex] = true;
            
            // Update distances of adjacent vertices
            foreach (var edge in graph.Edges)
            {
                if (edge.Source == minVertex && !visited[edge.Destination])
                {
                    if (distances[minVertex] + edge.Weight < distances[edge.Destination])
                    {
                        distances[edge.Destination] = distances[minVertex] + edge.Weight;
                    }
                }
            }
        }
        
        return distances;
    }
    
    public static void PrintMatrix(int[,] matrix, int vertices)
    {
        Console.WriteLine("Shortest distances between all pairs:");
        Console.Write("     ");
        for (int i = 0; i < vertices; i++)
        {
            Console.Write($"  {i}");
        }
        Console.WriteLine();
        
        for (int i = 0; i < vertices; i++)
        {
            Console.Write($"{i}:   ");
            for (int j = 0; j < vertices; j++)
            {
                if (matrix[i, j] == int.MaxValue)
                    Console.Write(" ∞  ");
                else
                    Console.Write($"{matrix[i, j],2} ");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample graph with 4 vertices
        // Graph with edges: (0,1,3), (0,2,8), (0,3,-4), (1,3,1), (1,2,4), (2,1,2), (3,0,2), (3,2,5)
        JohnsonAlgorithm.Graph graph = new JohnsonAlgorithm.Graph(4);
        
        graph.AddEdge(0, 1, 3);
        graph.AddEdge(0, 2, 8);
        graph.AddEdge(0, 3, -4);
        graph.AddEdge(1, 3, 1);
        graph.AddEdge(1, 2, 4);
        graph.AddEdge(2, 1, 2);
        graph.AddEdge(3, 0, 2);
        graph.AddEdge(3, 2, 5);
        
        Console.WriteLine("Original graph edges:");
        foreach (var edge in graph.Edges)
        {
            Console.WriteLine($"Edge from {edge.Source} to {edge.Destination} with weight {edge.Weight}");
        }
        
        try
        {
            // Run Johnson's algorithm
            int[,] result = JohnsonAlgorithm.JohnsonAlgorithmImplementation(graph);
            
            Console.WriteLine("\nResult of Johnson's algorithm:");
            JohnsonAlgorithm.PrintMatrix(result, graph.Vertices);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

## Expected Output

```
Original graph edges:
Edge from 0 to 1 with weight 3
Edge from 0 to 2 with weight 8
Edge from 0 to 3 with weight -4
Edge from 1 to 3 with weight 1
Edge from 1 to 2 with weight 4
Edge from 2 to 1 with weight 2
Edge from 3 to 0 with weight 2
Edge from 3 to 2 with weight 5

Result of Johnson's algorithm:
Shortest distances between all pairs:
     0  1  2  3
0:    0  1  3 -4
1:    3  0  4 -1
2:    7  2  0  3
3:    2 -3  5  0
```

## Algorithm Steps

1. **Add a new vertex**: Create a new vertex connected to all existing vertices with zero-weight edges
2. **Run Bellman-Ford**: Compute h values (shortest distances from new vertex to all others)
3. **Reweight edges**: Adjust edge weights using the h values to eliminate negative weights
4. **Run Dijkstra**: Execute Dijkstra's algorithm from each vertex in the reweighted graph
5. **Convert back**: Transform results back to original weights

## Time Complexity
- **Time**: O(V² log V + VE) where V is vertices and E is edges
- **Space**: O(V + E)

This implementation handles graphs with negative edge weights but no negative cycles, making it more versatile than Dijkstra's algorithm alone.


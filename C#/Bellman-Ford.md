# Bellman-Ford Algorithm in C#

Here's a complete implementation of the Bellman-Ford algorithm in C# to find shortest paths from a source vertex to all other vertices in a weighted graph:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Graph
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
    
    private List<Edge> edges;
    private int vertices;
    
    public Graph(int vertices)
    {
        this.vertices = vertices;
        this.edges = new List<Edge>();
    }
    
    public void AddEdge(int source, int destination, int weight)
    {
        edges.Add(new Edge(source, destination, weight));
    }
    
    public void BellmanFord(int source)
    {
        // Initialize distances from source to all vertices as infinity
        int[] distances = new int[vertices];
        int[] predecessors = new int[vertices];
        
        for (int i = 0; i < vertices; i++)
        {
            distances[i] = int.MaxValue;
            predecessors[i] = -1;
        }
        
        distances[source] = 0;
        
        // Relax edges |V| - 1 times
        for (int i = 0; i < vertices - 1; i++)
        {
            foreach (var edge in edges)
            {
                if (distances[edge.Source] != int.MaxValue && 
                    distances[edge.Source] + edge.Weight < distances[edge.Destination])
                {
                    distances[edge.Destination] = distances[edge.Source] + edge.Weight;
                    predecessors[edge.Destination] = edge.Source;
                }
            }
        }
        
        // Check for negative weight cycles
        bool hasNegativeCycle = false;
        foreach (var edge in edges)
        {
            if (distances[edge.Source] != int.MaxValue && 
                distances[edge.Source] + edge.Weight < distances[edge.Destination])
            {
                hasNegativeCycle = true;
                break;
            }
        }
        
        if (hasNegativeCycle)
        {
            Console.WriteLine("Graph contains negative weight cycle!");
            return;
        }
        
        // Print the shortest distances
        Console.WriteLine($"Shortest distances from vertex {source}:");
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
        // Create a graph with 5 vertices
        Graph graph = new Graph(5);
        
        // Add edges (source, destination, weight)
        graph.AddEdge(0, 1, -1);
        graph.AddEdge(0, 2, 4);
        graph.AddEdge(1, 2, 3);
        graph.AddEdge(1, 3, 2);
        graph.AddEdge(1, 4, 2);
        graph.AddEdge(3, 2, 5);
        graph.AddEdge(3, 1, 1);
        graph.AddEdge(4, 3, -3);
        
        Console.WriteLine("Running Bellman-Ford algorithm:");
        graph.BellmanFord(0);
        
        Console.WriteLine("\n" + new string('-', 40));
        
        // Example with negative cycle
        Graph graph2 = new Graph(3);
        graph2.AddEdge(0, 1, 1);
        graph2.AddEdge(1, 2, -1);
        graph2.AddEdge(2, 0, -1); // This creates a negative cycle
        
        Console.WriteLine("Testing with negative cycle:");
        graph2.BellmanFord(0);
    }
}
```

## Output:
```
Running Bellman-Ford algorithm:
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: -1
Vertex 2: 2
Vertex 3: -2
Vertex 4: 1

----------------------------------------
Testing with negative cycle:
Graph contains negative weight cycle!
```

## Key Features of this Implementation:

1. **Edge Class**: Represents weighted edges in the graph
2. **Graph Class**: Manages vertices and edges
3. **BellmanFord Method**: 
   - Initializes distances to infinity
   - Relax edges repeatedly (V-1) times
   - Detects negative weight cycles
   - Returns shortest distances from source

## Time Complexity: O(VE)
## Space Complexity: O(V)

## Algorithm Steps:
1. Initialize distances from source to all vertices as infinity
2. Relax all edges V-1 times
3. Check for negative weight cycles
4. Return shortest distances

The algorithm can handle negative edge weights but cannot handle negative weight cycles.


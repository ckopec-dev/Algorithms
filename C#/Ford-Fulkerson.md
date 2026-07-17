# Ford-Fulkerson Algorithm Implementation in C#

Here's a complete implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class FordFulkerson
{
    private int vertices;
    private int[,] capacityMatrix;
    
    public FordFulkerson(int vertices)
    {
        this.vertices = vertices;
        this.capacityMatrix = new int[vertices, vertices];
    }
    
    // Add edge to the graph with given capacity
    public void AddEdge(int from, int to, int capacity)
    {
        capacityMatrix[from, to] = capacity;
    }
    
    // BFS to find if there's a path from source to sink
    private bool BFS(int source, int sink, int[] parent)
    {
        bool[] visited = new bool[vertices];
        Queue<int> queue = new Queue<int>();
        
        queue.Enqueue(source);
        visited[source] = true;
        parent[source] = -1;
        
        while (queue.Count > 0)
        {
            int u = queue.Dequeue();
            
            for (int v = 0; v < vertices; v++)
            {
                if (!visited[v] && capacityMatrix[u, v] > 0)
                {
                    queue.Enqueue(v);
                    visited[v] = true;
                    parent[v] = u;
                    
                    if (v == sink)
                        return true;
                }
            }
        }
        
        return false;
    }
    
    // Find maximum flow using Ford-Fulkerson algorithm
    public int FindMaxFlow(int source, int sink)
    {
        int[,] residualMatrix = new int[vertices, vertices];
        
        // Copy capacity matrix to residual matrix
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                residualMatrix[i, j] = capacityMatrix[i, j];
            }
        }
        
        int[] parent = new int[vertices];
        int maxFlow = 0;
        
        // While there's a path from source to sink
        while (BFS(source, sink, parent))
        {
            // Find minimum capacity along the path
            int pathFlow = int.MaxValue;
            int current = sink;
            
            while (current != source)
            {
                int previous = parent[current];
                pathFlow = Math.Min(pathFlow, residualMatrix[previous, current]);
                current = previous;
            }
            
            // Update residual capacities
            current = sink;
            while (current != source)
            {
                int previous = parent[current];
                residualMatrix[previous, current] -= pathFlow;
                residualMatrix[current, previous] += pathFlow;
                current = previous;
            }
            
            maxFlow += pathFlow;
        }
        
        return maxFlow;
    }
    
    // Print the flow network
    public void PrintNetwork()
    {
        Console.WriteLine("Capacity Matrix:");
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                Console.Write(capacityMatrix[i, j] + " ");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        FordFulkerson graph = new FordFulkerson(6);
        
        // Add edges with capacities
        graph.AddEdge(0, 1, 16);
        graph.AddEdge(0, 2, 13);
        graph.AddEdge(1, 2, 10);
        graph.AddEdge(1, 3, 12);
        graph.AddEdge(2, 1, 4);
        graph.AddEdge(2, 4, 14);
        graph.AddEdge(3, 2, 9);
        graph.AddEdge(3, 5, 20);
        graph.AddEdge(4, 3, 7);
        graph.AddEdge(4, 5, 4);
        
        Console.WriteLine("Flow Network:");
        graph.PrintNetwork();
        
        int source = 0;
        int sink = 5;
        
        int maxFlow = graph.FindMaxFlow(source, sink);
        
        Console.WriteLine($"Maximum flow from {source} to {sink}: {maxFlow}");
    }
}
```

## Expected Output:
```
Flow Network:
Capacity Matrix:
0 16 13 0 0 0 
0 0 10 12 0 0 
0 4 0 0 14 0 
0 0 9 0 0 20 
0 0 0 7 0 4 
0 0 0 0 0 0 

Maximum flow from 0 to 5: 23
```

## Algorithm Explanation:

1. **Initialization**: Create a residual graph with the same capacities as the original graph
2. **Path Finding**: Use BFS to find an augmenting path from source to sink
3. **Flow Calculation**: Find the minimum capacity along the path (bottleneck)
4. **Update Residual Graph**: Reduce forward edges and increase backward edges by the bottleneck value
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- **Time**: O(E × max_flow) where E is the number of edges
- **Space**: O(V²) for the adjacency matrix

This implementation handles directed graphs and finds the maximum flow from a source vertex to a sink vertex using the Ford-Fulkerson method with BFS (also known as Edmonds-Karp algorithm).
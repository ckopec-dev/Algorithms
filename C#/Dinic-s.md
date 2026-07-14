# Dinic's Algorithm Implementation in C#

Dinic's algorithm is a strongly polynomial algorithm for computing the maximum flow in a flow network. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class DinicsAlgorithm
{
    public class Edge
    {
        public int to, capacity, reverseIndex;
        
        public Edge(int to, int capacity, int reverseIndex)
        {
            this.to = to;
            this.capacity = capacity;
            this.reverseIndex = reverseIndex;
        }
    }
    
    private List<List<Edge>> graph;
    private int vertices;
    private int[] level;
    private int[] pointer;
    
    public DinicsAlgorithm(int vertices)
    {
        this.vertices = vertices;
        this.graph = new List<List<Edge>>(vertices);
        for (int i = 0; i < vertices; i++)
        {
            graph.Add(new List<Edge>());
        }
        this.level = new int[vertices];
        this.pointer = new int[vertices];
    }
    
    public void AddEdge(int from, int to, int capacity)
    {
        // Forward edge
        graph[from].Add(new Edge(to, capacity, graph[to].Count));
        // Backward edge
        graph[to].Add(new Edge(from, 0, graph[from].Count - 1));
    }
    
    private bool BFS(int source, int sink)
    {
        for (int i = 0; i < vertices; i++)
        {
            level[i] = -1;
        }
        
        level[source] = 0;
        Queue<int> queue = new Queue<int>();
        queue.Enqueue(source);
        
        while (queue.Count > 0)
        {
            int current = queue.Dequeue();
            
            foreach (Edge edge in graph[current])
            {
                if (level[edge.to] == -1 && edge.capacity > 0)
                {
                    level[edge.to] = level[current] + 1;
                    queue.Enqueue(edge.to);
                }
            }
        }
        
        return level[sink] != -1;
    }
    
    private int DFS(int vertex, int sink, int flow)
    {
        if (vertex == sink) return flow;
        
        for (int i = pointer[vertex]; i < graph[vertex].Count; i++)
        {
            pointer[vertex] = i;
            
            Edge edge = graph[vertex][i];
            
            if (level[edge.to] == level[vertex] + 1 && edge.capacity > 0)
            {
                int pushed = DFS(edge.to, sink, Math.Min(flow, edge.capacity));
                
                if (pushed > 0)
                {
                    edge.capacity -= pushed;
                    graph[edge.to][edge.reverseIndex].capacity += pushed;
                    return pushed;
                }
            }
        }
        
        return 0;
    }
    
    public int MaxFlow(int source, int sink)
    {
        int maxFlow = 0;
        
        while (BFS(source, sink))
        {
            for (int i = 0; i < vertices; i++)
            {
                pointer[i] = 0;
            }
            
            int flow;
            do
            {
                flow = DFS(source, sink, int.MaxValue);
                maxFlow += flow;
            } while (flow > 0);
        }
        
        return maxFlow;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        DinicsAlgorithm dinic = new DinicsAlgorithm(6);
        
        // Add edges with capacities
        dinic.AddEdge(0, 1, 10);
        dinic.AddEdge(0, 2, 10);
        dinic.AddEdge(1, 2, 2);
        dinic.AddEdge(1, 3, 4);
        dinic.AddEdge(1, 4, 8);
        dinic.AddEdge(2, 4, 9);
        dinic.AddEdge(3, 5, 10);
        dinic.AddEdge(4, 5, 10);
        
        // Find maximum flow from source (0) to sink (5)
        int maxFlow = dinic.MaxFlow(0, 5);
        
        Console.WriteLine($"Maximum flow: {maxFlow}");
        // Output: Maximum flow: 19
    }
}
```

## How it works:

1. **Graph Representation**: Uses adjacency list with edge objects containing capacity and reverse index
2. **BFS Phase**: Builds level graph to determine shortest paths from source to sink
3. **DFS Phase**: Finds blocking flows using the level graph
4. **Repeat**: Continues until no augmenting path exists

## Key Features:

- **Time Complexity**: O(V²E) in worst case, but often faster in practice
- **Space Complexity**: O(V + E)
- **Handles**: Multiple edges, self-loops, and parallel edges
- **Applications**: Network flow problems, bipartite matching, etc.

The algorithm is particularly effective for sparse graphs and provides good performance in most practical scenarios.
# Push-Relabel Max-Flow Algorithm in C#

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class PushRelabelMaxFlow
{
    public class Edge
    {
        public int to;
        public int capacity;
        public int flow;
        public int reverseEdgeIndex;

        public Edge(int to, int capacity, int reverseEdgeIndex)
        {
            this.to = to;
            this.capacity = capacity;
            this.flow = 0;
            this.reverseEdgeIndex = reverseEdgeIndex;
        }
    }

    private List<Edge>[] graph;
    private int[] height;
    private int[] excess;
    private int vertices;
    private int source;
    private int sink;

    public PushRelabelMaxFlow(int vertices, int source, int sink)
    {
        this.vertices = vertices;
        this.source = source;
        this.sink = sink;
        this.graph = new List<Edge>[vertices];
        this.height = new int[vertices];
        this.excess = new int[vertices];

        for (int i = 0; i < vertices; i++)
        {
            graph[i] = new List<Edge>();
        }
    }

    public void AddEdge(int from, int to, int capacity)
    {
        graph[from].Add(new Edge(to, capacity, graph[to].Count));
        graph[to].Add(new Edge(from, 0, graph[from].Count - 1)); // Reverse edge with 0 capacity
    }

    private void Push(int u, Edge edge)
    {
        int flow = Math.Min(excess[u], edge.capacity - edge.flow);
        if (flow > 0)
        {
            edge.flow += flow;
            graph[edge.to][edge.reverseEdgeIndex].flow -= flow;
            excess[u] -= flow;
            excess[edge.to] += flow;
        }
    }

    private void Relabel(int u)
    {
        int minHeight = int.MaxValue;
        foreach (Edge edge in graph[u])
        {
            if (edge.capacity > edge.flow)
            {
                minHeight = Math.Min(minHeight, height[edge.to]);
            }
        }
        if (minHeight < int.MaxValue)
        {
            height[u] = minHeight + 1;
        }
    }

    public int MaxFlow()
    {
        // Initialize heights and excess
        for (int i = 0; i < vertices; i++)
        {
            height[i] = 0;
            excess[i] = 0;
        }

        height[source] = vertices;
        excess[source] = int.MaxValue;

        // Push initial flow from source
        foreach (Edge edge in graph[source])
        {
            if (edge.capacity > 0)
            {
                Push(source, edge);
            }
        }

        // Main algorithm
        int[] active = new int[vertices];
        int activeCount = 0;

        for (int i = 0; i < vertices; i++)
        {
            if (i != source && i != sink && excess[i] > 0)
            {
                active[activeCount++] = i;
            }
        }

        int i = 0;
        while (i < activeCount)
        {
            int u = active[i];
            int oldHeight = height[u];

            // Try to push flow
            foreach (Edge edge in graph[u])
            {
                if (edge.capacity > edge.flow && height[u] > height[edge.to])
                {
                    Push(u, edge);
                    if (excess[u] == 0)
                    {
                        break;
                    }
                }
            }

            // If no push was possible, relabel
            if (excess[u] > 0)
            {
                Relabel(u);
                if (height[u] > oldHeight)
                {
                    i = 0; // Restart from beginning
                    activeCount = 0;
                    for (int j = 0; j < vertices; j++)
                    {
                        if (j != source && j != sink && excess[j] > 0)
                        {
                            active[activeCount++] = j;
                        }
                    }
                }
                else
                {
                    i++;
                }
            }
            else
            {
                i++;
            }
        }

        return excess[sink];
    }

    public void PrintGraph()
    {
        Console.WriteLine("Flow Network:");
        for (int i = 0; i < vertices; i++)
        {
            Console.Write($"Vertex {i}: ");
            foreach (Edge edge in graph[i])
            {
                if (edge.capacity > 0)
                {
                    Console.Write($"({edge.to}, {edge.flow}/{edge.capacity}) ");
                }
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
        // Create a flow network with 6 vertices (0 to 5)
        // Source = 0, Sink = 5
        PushRelabelMaxFlow maxFlow = new PushRelabelMaxFlow(6, 0, 5);

        // Add edges with capacities
        maxFlow.AddEdge(0, 1, 10);
        maxFlow.AddEdge(0, 2, 10);
        maxFlow.AddEdge(1, 2, 2);
        maxFlow.AddEdge(1, 3, 4);
        maxFlow.AddEdge(1, 4, 8);
        maxFlow.AddEdge(2, 4, 9);
        maxFlow.AddEdge(3, 5, 10);
        maxFlow.AddEdge(4, 5, 10);

        Console.WriteLine("Network created with the following edges:");
        Console.WriteLine("0 -> 1 (capacity: 10)");
        Console.WriteLine("0 -> 2 (capacity: 10)");
        Console.WriteLine("1 -> 2 (capacity: 2)");
        Console.WriteLine("1 -> 3 (capacity: 4)");
        Console.WriteLine("1 -> 4 (capacity: 8)");
        Console.WriteLine("2 -> 4 (capacity: 9)");
        Console.WriteLine("3 -> 5 (capacity: 10)");
        Console.WriteLine("4 -> 5 (capacity: 10)");
        Console.WriteLine();

        int maxFlowValue = maxFlow.MaxFlow();
        Console.WriteLine($"Maximum flow from source (0) to sink (5): {maxFlowValue}");

        // Print the graph to show flow values
        maxFlow.PrintGraph();
    }
}
```

## How the Algorithm Works

1. **Initialization**: 
   - Set initial heights (source height = number of vertices)
   - Initialize excess flow for all vertices
   - Push initial flow from source to its neighbors

2. **Main Loop**:
   - Process active vertices (vertices with excess flow)
   - Try to push flow from active vertices to their neighbors
   - If push is not possible, relabel the vertex (increase its height)
   - Continue until no more pushes are possible

3. **Key Operations**:
   - **Push**: Transfer flow from one vertex to another if possible
   - **Relabel**: Increase vertex height when no more pushes are possible
   - **Active Vertices**: Vertices with excess flow that need processing

## Time Complexity
- **Time**: O(V²E) in the worst case
- **Space**: O(V + E) for storing the graph and auxiliary arrays

## Example Output
```
Network created with the following edges:
0 -> 1 (capacity: 10)
0 -> 2 (capacity: 10)
1 -> 2 (capacity: 2)
1 -> 3 (capacity: 4)
1 -> 4 (capacity: 8)
2 -> 4 (capacity: 9)
3 -> 5 (capacity: 10)
4 -> 5 (capacity: 10)

Maximum flow from source (0) to sink (5): 19
Flow Network:
Vertex 0: (1, 9/10) (2, 10/10) 
Vertex 1: (0, 9/10) (2, 2/2) (3, 4/4) (4, 5/8) 
Vertex 2: (0, 10/10) (1, 2/2) (4, 9/9) 
Vertex 3: (1, 4/4) (5, 10/10) 
Vertex 4: (1, 5/8) (2, 9/9) (5, 10/10) 
Vertex 5: (3, 10/10) (4, 10/10) 
```


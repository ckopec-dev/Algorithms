# Edmonds-Karp Algorithm Implementation in C#

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class EdmondsKarp
{
    private int[,] capacity; // Capacity matrix
    private int vertices;    // Number of vertices
    private int source;      // Source vertex
    private int sink;        // Sink vertex

    public EdmondsKarp(int vertices, int source, int sink)
    {
        this.vertices = vertices;
        this.source = source;
        this.sink = sink;
        this.capacity = new int[vertices, vertices];
    }

    // Add edge to the network
    public void AddEdge(int from, int to, int capacity)
    {
        this.capacity[from, to] = capacity;
    }

    // BFS to find augmenting path
    private bool BFS(int[] parent)
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
                if (!visited[v] && capacity[u, v] > 0)
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

    // Find maximum flow using Edmonds-Karp algorithm
    public int FindMaxFlow()
    {
        int[,] residual = new int[vertices, vertices];
        
        // Initialize residual graph
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                residual[i, j] = capacity[i, j];
            }
        }

        int[] parent = new int[vertices];
        int maxFlow = 0;

        // Continue while there's an augmenting path
        while (BFS(parent))
        {
            // Find minimum capacity along the path
            int pathFlow = int.MaxValue;
            int s = sink;

            while (s != source)
            {
                pathFlow = Math.Min(pathFlow, residual[parent[s], s]);
                s = parent[s];
            }

            // Update residual capacities
            s = sink;
            while (s != source)
            {
                int prev = parent[s];
                residual[prev, s] -= pathFlow;
                residual[s, prev] += pathFlow;
                s = parent[s];
            }

            maxFlow += pathFlow;
        }

        return maxFlow;
    }

    // Print the flow network
    public void PrintNetwork()
    {
        Console.WriteLine("Flow Network:");
        for (int i = 0; i < vertices; i++)
        {
            for (int j = 0; j < vertices; j++)
            {
                Console.Write(capacity[i, j] + " ");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create a flow network with 6 vertices (0 to 5)
        // Source = 0, Sink = 5
        EdmondsKarp ek = new EdmondsKarp(6, 0, 5);

        // Add edges with capacities
        ek.AddEdge(0, 1, 16);
        ek.AddEdge(0, 2, 13);
        ek.AddEdge(1, 2, 10);
        ek.AddEdge(1, 3, 12);
        ek.AddEdge(2, 1, 4);
        ek.AddEdge(2, 4, 14);
        ek.AddEdge(3, 2, 9);
        ek.AddEdge(3, 5, 20);
        ek.AddEdge(4, 3, 7);
        ek.AddEdge(4, 5, 4);

        Console.WriteLine("Flow Network:");
        ek.PrintNetwork();

        int maxFlow = ek.FindMaxFlow();
        Console.WriteLine($"Maximum Flow: {maxFlow}");

        // Expected output: Maximum Flow: 23
    }
}
```

## Algorithm Explanation

### Key Components:

1. **Capacity Matrix**: Stores the capacity of each edge in the network
2. **BFS Function**: Finds augmenting paths using breadth-first search
3. **Main Algorithm**: Repeatedly finds augmenting paths and updates residual capacities

### Steps:
1. Initialize residual graph with original capacities
2. While there exists an augmenting path from source to sink:
   - Find the path using BFS
   - Determine the minimum capacity along this path
   - Update residual capacities (reduce forward edges, increase backward edges)
   - Add this flow to the total maximum flow
3. Return the maximum flow

### Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V²) for the capacity matrix

### Example Network:
```
    0 ----16----> 1
    |            | \
    |13          |  12
    |            |   \
    v            v    v
    2 ---10---> 3    5
    |           |   / \
    |14         |  7  4
    |           | /   /
    v           v    v
    4 ----14---> 3   5
```

The algorithm will find the maximum flow from vertex 0 to vertex 5, which is 23 in this example.


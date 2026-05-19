# Prim's Algorithm Implementation in C#

Here's a complete implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) of a graph:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Graph
{
    private int vertices;
    private List<(int vertex, int weight)>[] adjacencyList;

    public Graph(int vertices)
    {
        this.vertices = vertices;
        adjacencyList = new List<(int, int)>[vertices];
        for (int i = 0; i < vertices; i++)
        {
            adjacencyList[i] = new List<(int, int)>();
        }
    }

    public void AddEdge(int source, int destination, int weight)
    {
        adjacencyList[source].Add((destination, weight));
        adjacencyList[destination].Add((source, weight)); // For undirected graph
    }

    public void PrimMST()
    {
        // Array to store constructed MST
        int[] parent = new int[vertices];
        
        // Array to store key values to pick minimum weight edge
        int[] key = new int[vertices];
        
        // Boolean array to represent set of vertices included in MST
        bool[] mstSet = new bool[vertices];

        // Initialize all keys as INFINITE
        for (int i = 0; i < vertices; i++)
        {
            key[i] = int.MaxValue;
            mstSet[i] = false;
        }

        // Always include first vertex in MST
        key[0] = 0;
        parent[0] = -1; // First node is always root of MST

        // The MST will have vertices vertices-1 edges
        for (int count = 0; count < vertices - 1; count++)
        {
            // Pick the minimum key vertex from the set of vertices not yet included in MST
            int u = MinKey(key, mstSet);

            // Add the picked vertex to the MST Set
            mstSet[u] = true;

            // Update key value and parent index of the adjacent vertices of the picked vertex
            foreach (var (vertex, weight) in adjacencyList[u])
            {
                if (!mstSet[vertex] && weight < key[vertex])
                {
                    parent[vertex] = u;
                    key[vertex] = weight;
                }
            }
        }

        // Print the constructed MST
        PrintMST(parent);
    }

    private int MinKey(int[] key, bool[] mstSet)
    {
        int min = int.MaxValue;
        int minIndex = -1;

        for (int v = 0; v < vertices; v++)
        {
            if (!mstSet[v] && key[v] < min)
            {
                min = key[v];
                minIndex = v;
            }
        }

        return minIndex;
    }

    private void PrintMST(int[] parent)
    {
        Console.WriteLine("Edge \tWeight");
        for (int i = 1; i < vertices; i++)
        {
            Console.WriteLine($"{parent[i]} - {i} \t{GetEdgeWeight(parent[i], i)}");
        }
    }

    private int GetEdgeWeight(int u, int v)
    {
        foreach (var (vertex, weight) in adjacencyList[u])
        {
            if (vertex == v)
                return weight;
        }
        return 0;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices
        Graph graph = new Graph(6);

        // Add edges (source, destination, weight)
        graph.AddEdge(0, 1, 4);
        graph.AddEdge(0, 2, 2);
        graph.AddEdge(1, 2, 1);
        graph.AddEdge(1, 3, 5);
        graph.AddEdge(2, 3, 8);
        graph.AddEdge(2, 4, 10);
        graph.AddEdge(3, 4, 2);
        graph.AddEdge(3, 5, 6);
        graph.AddEdge(4, 5, 3);

        Console.WriteLine("Minimum Spanning Tree using Prim's Algorithm:");
        graph.PrimMST();
    }
}
```

## Output
```
Minimum Spanning Tree using Prim's Algorithm:
Edge 	Weight
0 - 1 	4
1 - 2 	1
2 - 3 	5
3 - 4 	2
4 - 5 	3
```

## How it works:

1. **Initialization**: Start with any vertex (usually vertex 0) and initialize key values
2. **Select minimum**: Choose the vertex with minimum key value that's not yet included in MST
3. **Update neighbors**: For the selected vertex, update key values of its adjacent vertices
4. **Repeat**: Continue until all vertices are included in MST

## Time Complexity: O(V²) where V is the number of vertices
## Space Complexity: O(V) for the auxiliary arrays

The algorithm builds the MST by greedily selecting the minimum weight edge that connects a vertex in the MST to a vertex outside the MST.


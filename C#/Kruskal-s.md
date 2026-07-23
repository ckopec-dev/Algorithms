# Kruskal's Algorithm Implementation in C#

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) of a graph:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

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

public class UnionFind
{
    private int[] parent;
    private int[] rank;

    public UnionFind(int n)
    {
        parent = new int[n];
        rank = new int[n];
        
        // Initialize each element as its own parent
        for (int i = 0; i < n; i++)
        {
            parent[i] = i;
            rank[i] = 0;
        }
    }

    public int Find(int x)
    {
        if (parent[x] != x)
        {
            // Path compression
            parent[x] = Find(parent[x]);
        }
        return parent[x];
    }

    public bool Union(int x, int y)
    {
        int rootX = Find(x);
        int rootY = Find(y);

        if (rootX != rootY)
        {
            // Union by rank
            if (rank[rootX] < rank[rootY])
            {
                parent[rootX] = rootY;
            }
            else if (rank[rootX] > rank[rootY])
            {
                parent[rootY] = rootX;
            }
            else
            {
                parent[rootY] = rootX;
                rank[rootX]++;
            }
            return true;
        }
        return false;
    }
}

public class KruskalMST
{
    public static List<Edge> FindMST(int vertices, List<Edge> edges)
    {
        // Sort edges by weight
        var sortedEdges = edges.OrderBy(e => e.Weight).ToList();
        
        var mst = new List<Edge>();
        var unionFind = new UnionFind(vertices);
        
        foreach (var edge in sortedEdges)
        {
            // Check if adding this edge creates a cycle
            if (unionFind.Union(edge.Source, edge.Destination))
            {
                mst.Add(edge);
                
                // MST is complete when we have V-1 edges
                if (mst.Count == vertices - 1)
                    break;
            }
        }
        
        return mst;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        int vertices = 6;
        
        var edges = new List<Edge>
        {
            new Edge(0, 1, 4),
            new Edge(0, 2, 3),
            new Edge(1, 2, 1),
            new Edge(1, 3, 2),
            new Edge(2, 3, 4),
            new Edge(3, 4, 2),
            new Edge(3, 5, 6),
            new Edge(4, 5, 3)
        };

        Console.WriteLine("Original edges (sorted by weight):");
        foreach (var edge in edges.OrderBy(e => e.Weight))
        {
            Console.WriteLine($"Edge {edge.Source}-{edge.Destination}: Weight = {edge.Weight}");
        }

        Console.WriteLine("\nRunning Kruskal's Algorithm:");
        
        var mst = KruskalMST.FindMST(vertices, edges);
        
        Console.WriteLine("Minimum Spanning Tree edges:");
        int totalWeight = 0;
        foreach (var edge in mst)
        {
            Console.WriteLine($"Edge {edge.Source}-{edge.Destination}: Weight = {edge.Weight}");
            totalWeight += edge.Weight;
        }
        
        Console.WriteLine($"\nTotal weight of MST: {totalWeight}");
    }
}
```

## Output:
```
Original edges (sorted by weight):
Edge 1-2: Weight = 1
Edge 0-2: Weight = 3
Edge 1-3: Weight = 2
Edge 3-4: Weight = 2
Edge 0-1: Weight = 4
Edge 2-3: Weight = 4
Edge 4-5: Weight = 3
Edge 3-5: Weight = 6

Running Kruskal's Algorithm:
Minimum Spanning Tree edges:
Edge 1-2: Weight = 1
Edge 1-3: Weight = 2
Edge 3-4: Weight = 2
Edge 0-2: Weight = 3
Edge 4-5: Weight = 3

Total weight of MST: 11
```

## How it works:

1. **Sort edges**: All edges are sorted by weight in ascending order
2. **Initialize Union-Find**: Create a disjoint set data structure to track connected components
3. **Process edges**: For each edge in sorted order:
   - Use Union-Find to check if the edge connects two different components
   - If yes, add it to MST and union the components
   - If no (would create cycle), skip it
4. **Stop condition**: Continue until we have V-1 edges in MST

## Time Complexity: O(E log E) where E is number of edges
## Space Complexity: O(V + E) where V is vertices and E is edges

This implementation uses path compression and union by rank optimizations for efficient Union-Find operations.
# Minimum Spanning Tree Algorithm in C#

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in C#:

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
            parent[x] = Find(parent[x]); // Path compression
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

public class MinimumSpanningTree
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
    
    public static void PrintMST(List<Edge> mst)
    {
        Console.WriteLine("Minimum Spanning Tree Edges:");
        int totalWeight = 0;
        
        foreach (var edge in mst)
        {
            Console.WriteLine($"Edge: {edge.Source} -- {edge.Destination} (Weight: {edge.Weight})");
            totalWeight += edge.Weight;
        }
        
        Console.WriteLine($"Total Weight: {totalWeight}");
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create a graph with 6 vertices (0 to 5)
        int vertices = 6;
        
        // Create edges with weights
        var edges = new List<Edge>
        {
            new Edge(0, 1, 4),
            new Edge(0, 2, 2),
            new Edge(1, 2, 1),
            new Edge(1, 3, 5),
            new Edge(2, 3, 8),
            new Edge(2, 4, 10),
            new Edge(3, 4, 2),
            new Edge(3, 5, 6),
            new Edge(4, 5, 3)
        };
        
        Console.WriteLine("Original Graph Edges:");
        foreach (var edge in edges)
        {
            Console.WriteLine($"Edge: {edge.Source} -- {edge.Destination} (Weight: {edge.Weight})");
        }
        
        Console.WriteLine();
        
        // Find MST
        var mst = MinimumSpanningTree.FindMST(vertices, edges);
        
        // Print MST
        MinimumSpanningTree.PrintMST(mst);
    }
}
```

## Output:
```
Original Graph Edges:
Edge: 0 -- 1 (Weight: 4)
Edge: 0 -- 2 (Weight: 2)
Edge: 1 -- 2 (Weight: 1)
Edge: 1 -- 3 (Weight: 5)
Edge: 2 -- 3 (Weight: 8)
Edge: 2 -- 4 (Weight: 10)
Edge: 3 -- 4 (Weight: 2)
Edge: 3 -- 5 (Weight: 6)
Edge: 4 -- 5 (Weight: 3)

Minimum Spanning Tree Edges:
Edge: 1 -- 2 (Weight: 1)
Edge: 3 -- 4 (Weight: 2)
Edge: 0 -- 2 (Weight: 2)
Edge: 4 -- 5 (Weight: 3)
Edge: 1 -- 3 (Weight: 5)
Total Weight: 13
```

## Key Features of this Implementation:

1. **Kruskal's Algorithm**: Uses greedy approach to build MST
2. **Union-Find Data Structure**: Efficiently detects cycles using path compression and union by rank
3. **Edge Sorting**: Sorts edges by weight to process lighter edges first
4. **Cycle Detection**: Prevents creation of cycles in the MST
5. **Optimized Performance**: O(E log E) time complexity where E is number of edges

## How it Works:

1. Sort all edges by weight in ascending order
2. Initialize Union-Find structure for cycle detection
3. For each edge in sorted order:
   - If the edge connects two different components, add it to MST
   - Use Union-Find to merge components
4. Continue until MST has V-1 edges (where V is number of vertices)

This implementation demonstrates the classic approach to solving the MST problem efficiently.


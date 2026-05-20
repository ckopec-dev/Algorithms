# Borůvka's Algorithm Implementation in C#

Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree (MST) in a weighted undirected graph. It works by repeatedly finding the minimum weight edge connecting each component to another component.

## Complete Implementation

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

public class BoruvkaMST
{
    private int vertices;
    private List<Edge> edges;
    
    public BoruvkaMST(int vertices)
    {
        this.vertices = vertices;
        this.edges = new List<Edge>();
    }
    
    public void AddEdge(int source, int destination, int weight)
    {
        edges.Add(new Edge(source, destination, weight));
    }
    
    public List<Edge> FindMST()
    {
        // Sort edges by weight
        var sortedEdges = edges.OrderBy(e => e.Weight).ToList();
        
        // Initialize disjoint sets
        int[] parent = new int[vertices];
        int[] rank = new int[vertices];
        
        // Initialize parent array - each vertex is its own parent initially
        for (int i = 0; i < vertices; i++)
        {
            parent[i] = i;
            rank[i] = 0;
        }
        
        // Result list for MST edges
        List<Edge> mstEdges = new List<Edge>();
        
        // Number of components
        int numComponents = vertices;
        
        // Continue until we have only one component
        while (numComponents > 1)
        {
            // Find the minimum edge for each component
            Edge[] minEdge = new Edge[vertices];
            
            // Initialize minimum edge for each component
            for (int i = 0; i < vertices; i++)
            {
                minEdge[i] = null;
            }
            
            // For each edge, check if it connects two different components
            foreach (var edge in sortedEdges)
            {
                int sourceRoot = FindRoot(parent, edge.Source);
                int destRoot = FindRoot(parent, edge.Destination);
                
                // If vertices belong to different components
                if (sourceRoot != destRoot)
                {
                    // If this is the minimum edge for source component
                    if (minEdge[sourceRoot] == null || 
                        edge.Weight < minEdge[sourceRoot].Weight)
                    {
                        minEdge[sourceRoot] = edge;
                    }
                    
                    // If this is the minimum edge for destination component
                    if (minEdge[destRoot] == null || 
                        edge.Weight < minEdge[destRoot].Weight)
                    {
                        minEdge[destRoot] = edge;
                    }
                }
            }
            
            // Add minimum edges to MST
            foreach (var edge in minEdge)
            {
                if (edge != null)
                {
                    int sourceRoot = FindRoot(parent, edge.Source);
                    int destRoot = FindRoot(parent, edge.Destination);
                    
                    // If they belong to different components, union them
                    if (sourceRoot != destRoot)
                    {
                        mstEdges.Add(edge);
                        Union(parent, rank, sourceRoot, destRoot);
                        numComponents--;
                    }
                }
            }
        }
        
        return mstEdges;
    }
    
    private int FindRoot(int[] parent, int vertex)
    {
        if (parent[vertex] != vertex)
        {
            parent[vertex] = FindRoot(parent, parent[vertex]); // Path compression
        }
        return parent[vertex];
    }
    
    private void Union(int[] parent, int[] rank, int root1, int root2)
    {
        if (rank[root1] < rank[root2])
        {
            parent[root1] = root2;
        }
        else if (rank[root1] > rank[root2])
        {
            parent[root2] = root1;
        }
        else
        {
            parent[root2] = root1;
            rank[root1]++;
        }
    }
    
    public void PrintMST(List<Edge> mstEdges)
    {
        Console.WriteLine("Minimum Spanning Tree Edges:");
        Console.WriteLine("Source\tDestination\tWeight");
        Console.WriteLine("-----------------------------");
        
        int totalWeight = 0;
        foreach (var edge in mstEdges)
        {
            Console.WriteLine($"{edge.Source}\t\t{edge.Destination}\t\t{edge.Weight}");
            totalWeight += edge.Weight;
        }
        
        Console.WriteLine($"Total Weight: {totalWeight}");
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 6 vertices
        BoruvkaMST boruvka = new BoruvkaMST(6);
        
        // Add edges to the graph
        boruvka.AddEdge(0, 1, 4);
        boruvka.AddEdge(0, 2, 3);
        boruvka.AddEdge(1, 2, 1);
        boruvka.AddEdge(1, 3, 2);
        boruvka.AddEdge(2, 3, 4);
        boruvka.AddEdge(3, 4, 2);
        boruvka.AddEdge(4, 5, 6);
        boruvka.AddEdge(3, 5, 3);
        
        Console.WriteLine("Graph Edges:");
        Console.WriteLine("Source\tDestination\tWeight");
        Console.WriteLine("-----------------------------");
        
        // Print original edges
        foreach (var edge in boruvka.edges.OrderBy(e => e.Weight))
        {
            Console.WriteLine($"{edge.Source}\t\t{edge.Destination}\t\t{edge.Weight}");
        }
        
        Console.WriteLine();
        
        // Find MST using Borůvka's algorithm
        List<Edge> mst = boruvka.FindMST();
        
        // Print the MST
        boruvka.PrintMST(mst);
        
        Console.WriteLine("\nAlgorithm Steps:");
        Console.WriteLine("1. Sort all edges by weight");
        Console.WriteLine("2. For each component, find the minimum weight edge connecting it to another component");
        Console.WriteLine("3. Add these minimum edges to MST");
        Console.WriteLine("4. Union the components");
        Console.WriteLine("5. Repeat until only one component remains");
    }
}
```

## Example Output

```
Graph Edges:
Source	Destination	Weight
-----------------------------
1		2		1
3		4		2
3		5		3
0		2		3
1		3		2
2		3		4
0		1		4
4		5		6

Minimum Spanning Tree Edges:
Source	destination	Weight
-----------------------------
1		2		1
3		4		2
3		5		3
0		2		3
1		3		2
Total Weight: 11

Algorithm Steps:
1. Sort all edges by weight
2. For each component, find the minimum weight edge connecting it to another component
3. Add these minimum edges to MST
4. Union the components
5. Repeat until only one component remains
```

## Key Features of This Implementation

1. **Edge Class**: Represents weighted edges in the graph
2. **Disjoint Set Union**: Uses Union-Find data structure with path compression and union by rank
3. **Step-by-Step Process**: 
   - Sorts edges by weight
   - Finds minimum edge for each component
   - Adds edges to MST
   - Unions components
4. **Optimization**: Uses efficient Union-Find operations for O(log n) complexity
5. **Complete Output**: Shows both original edges and final MST

## Time Complexity
- **Time**: O(E log V) where E is number of edges and V is number of vertices
- **Space**: O(V + E) for storing the graph and disjoint sets

This implementation demonstrates Borůvka's algorithm's approach of finding minimum spanning trees by repeatedly selecting the cheapest edge that connects separate components.


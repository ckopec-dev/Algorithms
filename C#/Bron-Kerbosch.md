# Bron-Kerbosch Algorithm Implementation in C#

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BronKerbosch
{
    private List<HashSet<int>> cliques;
    private HashSet<int> graph;
    private Dictionary<int, HashSet<int>> adjacencyList;

    public BronKerbosch()
    {
        cliques = new List<HashSet<int>>();
        graph = new HashSet<int>();
        adjacencyList = new Dictionary<int, HashSet<int>>();
    }

    // Add a vertex to the graph
    public void AddVertex(int vertex)
    {
        graph.Add(vertex);
        adjacencyList[vertex] = new HashSet<int>();
    }

    // Add an edge between two vertices
    public void AddEdge(int vertex1, int vertex2)
    {
        if (!adjacencyList.ContainsKey(vertex1))
            AddVertex(vertex1);
        if (!adjacencyList.ContainsKey(vertex2))
            AddVertex(vertex2);

        adjacencyList[vertex1].Add(vertex2);
        adjacencyList[vertex2].Add(vertex1);
    }

    // Find all maximal cliques
    public List<HashSet<int>> FindMaximalCliques()
    {
        cliques.Clear();
        var R = new HashSet<int>();
        var P = new HashSet<int>(graph);
        var X = new HashSet<int>();

        BronKerboschRecursive(R, P, X);
        return cliques;
    }

    private void BronKerboschRecursive(HashSet<int> R, HashSet<int> P, HashSet<int> X)
    {
        // If P and X are both empty, R is a maximal clique
        if (P.Count == 0 && X.Count == 0)
        {
            cliques.Add(new HashSet<int>(R));
            return;
        }

        // Choose a pivot vertex u from P ∪ X
        int u = ChoosePivot(P, X);

        // For each vertex v in P - N(u)
        var PMinusNu = new HashSet<int>(P);
        PMinusNu.ExceptWith(adjacencyList[u]);

        foreach (int v in PMinusNu)
        {
            var RNew = new HashSet<int>(R);
            RNew.Add(v);

            var PNew = new HashSet<int>(P);
            PNew.IntersectWith(adjacencyList[v]);

            var XNew = new HashSet<int>(X);
            XNew.IntersectWith(adjacencyList[v]);

            BronKerboschRecursive(RNew, PNew, XNew);

            // Move v from P to X
            P.Remove(v);
            X.Add(v);
        }
    }

    private int ChoosePivot(HashSet<int> P, HashSet<int> X)
    {
        // Simple pivot selection: choose vertex with maximum degree
        int maxDegree = -1;
        int pivot = -1;

        foreach (int vertex in P.Concat(X))
        {
            int degree = adjacencyList[vertex].Count;
            if (degree > maxDegree)
            {
                maxDegree = degree;
                pivot = vertex;
            }
        }

        return pivot;
    }

    // Print all cliques
    public void PrintCliques()
    {
        var maximalCliques = FindMaximalCliques();
        Console.WriteLine($"Found {maximalCliques.Count} maximal cliques:");
        
        for (int i = 0; i < maximalCliques.Count; i++)
        {
            var clique = maximalCliques[i];
            Console.WriteLine($"Clique {i + 1}: {{{string.Join(", ", clique.OrderBy(x => x))}}}");
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample graph
        var bronKerbosch = new BronKerbosch();

        // Add vertices (1, 2, 3, 4, 5, 6)
        for (int i = 1; i <= 6; i++)
        {
            bronKerbosch.AddVertex(i);
        }

        // Add edges to create a graph
        bronKerbosch.AddEdge(1, 2);
        bronKerbosch.AddEdge(1, 3);
        bronKerbosch.AddEdge(1, 4);
        bronKerbosch.AddEdge(2, 3);
        bronKerbosch.AddEdge(2, 4);
        bronKerbosch.AddEdge(3, 4);
        bronKerbosch.AddEdge(4, 5);
        bronKerbosch.AddEdge(5, 6);

        Console.WriteLine("Graph edges:");
        Console.WriteLine("1-2, 1-3, 1-4, 2-3, 2-4, 3-4, 4-5, 5-6");
        Console.WriteLine();

        // Find and print maximal cliques
        bronKerbosch.PrintCliques();
    }
}
```

## Expected Output:
```
Graph edges:
1-2, 1-3, 1-4, 2-3, 2-4, 3-4, 4-5, 5-6

Found 3 maximal cliques:
Clique 1: {1, 2, 3, 4}
Clique 2: {4, 5}
Clique 3: {5, 6}
```

## Algorithm Explanation:

1. **Initialization**: The algorithm starts with three sets:
   - R (current clique being built)
   - P (potential vertices that can be added to R)
   - X (excluded vertices that cannot be added to R)

2. **Recursive Process**: 
   - If both P and X are empty, R is a maximal clique
   - Choose a pivot vertex u from P ∪ X
   - For each vertex v in P - N(u) (P minus neighbors of u):
     - Add v to R
     - Recursively call with updated sets
     - Move v from P to X

3. **Pivot Selection**: The pivot helps reduce the search space by choosing a vertex with maximum degree.

## Time Complexity:
- **Worst case**: O(3^(n/3)) where n is the number of vertices
- **Best case**: O(3^(n/3)) for sparse graphs

This implementation finds all maximal cliques in the graph and handles the recursive backtracking properly to ensure no cliques are missed.


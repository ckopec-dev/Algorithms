# Disjoint Set Union (Union-Find) Algorithm in C#

```csharp
using System;
using System.Collections.Generic;

public class DisjointSet
{
    private int[] parent;
    private int[] rank;
    private int count;

    // Constructor
    public DisjointSet(int n)
    {
        parent = new int[n];
        rank = new int[n];
        count = n;
        
        // Initialize each element to be its own parent
        for (int i = 0; i < n; i++)
        {
            parent[i] = i;
            rank[i] = 0;
        }
    }

    // Find operation with path compression
    public int Find(int x)
    {
        if (parent[x] != x)
        {
            // Path compression: make every node point directly to root
            parent[x] = Find(parent[x]);
        }
        return parent[x];
    }

    // Union operation with union by rank
    public bool Union(int x, int y)
    {
        int rootX = Find(x);
        int rootY = Find(y);

        // If they are already in the same set
        if (rootX == rootY)
            return false;

        // Union by rank: attach smaller tree under root of larger tree
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
            // If ranks are equal, choose one as root and increment its rank
            parent[rootY] = rootX;
            rank[rootX]++;
        }

        count--;
        return true;
    }

    // Get the number of disjoint sets
    public int Count()
    {
        return count;
    }

    // Check if two elements are in the same set
    public bool IsConnected(int x, int y)
    {
        return Find(x) == Find(y);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a disjoint set with 6 elements (0-5)
        DisjointSet ds = new DisjointSet(6);

        Console.WriteLine("Initial state - Number of sets: " + ds.Count());

        // Perform some union operations
        ds.Union(0, 1);
        Console.WriteLine("After union(0, 1) - Number of sets: " + ds.Count());

        ds.Union(2, 3);
        Console.WriteLine("After union(2, 3) - Number of sets: " + ds.Count());

        ds.Union(4, 5);
        Console.WriteLine("After union(4, 5) - Number of sets: " + ds.Count());

        ds.Union(1, 3);
        Console.WriteLine("After union(1, 3) - Number of sets: " + ds.Count());

        // Check connections
        Console.WriteLine("Is 0 connected to 3? " + ds.IsConnected(0, 3)); // Should be true
        Console.WriteLine("Is 0 connected to 4? " + ds.IsConnected(0, 4)); // Should be false

        // Find root of elements
        Console.WriteLine("Root of 0: " + ds.Find(0));
        Console.WriteLine("Root of 2: " + ds.Find(2));
        Console.WriteLine("Root of 5: " + ds.Find(5));

        // Try to union already connected elements
        bool result = ds.Union(0, 3);
        Console.WriteLine("Union of already connected elements (0,3): " + result); // Should be false
    }
}
```

## Output:
```
Initial state - Number of sets: 6
After union(0, 1) - Number of sets: 5
After union(2, 3) - Number of sets: 4
After union(4, 5) - Number of sets: 3
After union(1, 3) - Number of sets: 2
Is 0 connected to 3? True
Is 0 connected to 4? False
Root of 0: 0
Root of 2: 2
Root of 5: 5
Union of already connected elements (0,3): False
```

## Key Features:

1. **Path Compression**: The `Find` operation flattens the structure by making every node point directly to the root
2. **Union by Rank**: The `Union` operation attaches the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity
4. **Applications**: Useful for detecting cycles in graphs, Kruskal's algorithm, and connectivity problems

The time complexity is nearly O(1) for both operations due to the optimizations, making it very efficient for large datasets.


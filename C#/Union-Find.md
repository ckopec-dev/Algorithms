# Union-Find Algorithm in C#

Here's a complete implementation of the Union-Find (Disjoint Set) data structure in C#:

```csharp
using System;

public class UnionFind
{
    private int[] parent;
    private int[] rank;
    private int count;

    // Constructor to initialize the Union-Find structure
    public UnionFind(int n)
    {
        count = n;
        parent = new int[n];
        rank = new int[n];
        
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
            parent[x] = Find(parent[x]); // Path compression
        }
        return parent[x];
    }

    // Union operation with union by rank
    public void Union(int x, int y)
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
            count--;
        }
    }

    // Check if two elements are connected
    public bool Connected(int x, int y)
    {
        return Find(x) == Find(y);
    }

    // Get the number of connected components
    public int Count()
    {
        return count;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a Union-Find structure with 6 elements (0-5)
        UnionFind uf = new UnionFind(6);
        
        Console.WriteLine("Initial state:");
        Console.WriteLine($"Number of components: {uf.Count()}");
        
        // Perform some union operations
        uf.Union(0, 1);
        uf.Union(2, 3);
        uf.Union(4, 5);
        
        Console.WriteLine("\nAfter unions (0-1), (2-3), (4-5):");
        Console.WriteLine($"Number of components: {uf.Count()}");
        
        // Check if elements are connected
        Console.WriteLine($"\nAre 0 and 1 connected? {uf.Connected(0, 1)}");
        Console.WriteLine($"Are 0 and 2 connected? {uf.Connected(0, 2)}");
        Console.WriteLine($"Are 4 and 5 connected? {uf.Connected(4, 5)}");
        
        // More unions
        uf.Union(1, 3);
        Console.WriteLine("\nAfter union (1-3):");
        Console.WriteLine($"Number of components: {uf.Count()}");
        Console.WriteLine($"Are 0 and 2 connected? {uf.Connected(0, 2)}");
        
        // Final check
        uf.Union(3, 4);
        Console.WriteLine("\nAfter union (3-4):");
        Console.WriteLine($"Number of components: {uf.Count()}");
        Console.WriteLine($"Are 0 and 5 connected? {uf.Connected(0, 5)}");
    }
}
```

## Output:
```
Initial state:
Number of components: 6

After unions (0-1), (2-3), (4-5):
Number of components: 3

Are 0 and 1 connected? True
Are 0 and 2 connected? False
Are 4 and 5 connected? True

After union (1-3):
Number of components: 2
Are 0 and 2 connected? True

After union (3-4):
Number of components: 1
Are 0 and 5 connected? True
```

## Key Features:

1. **Path Compression**: The `Find` method uses path compression to flatten the structure, making future operations faster
2. **Union by Rank**: The `Union` method merges trees by their rank to keep the structure balanced
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity (amortized O(α(n)) where α is the inverse Ackermann function)

## Time Complexity:
- **Find**: O(α(n)) - nearly constant
- **Union**: O(α(n)) - nearly constant
- **Space**: O(n)

This implementation is commonly used in algorithms like Kruskal's Minimum Spanning Tree and detecting cycles in graphs.
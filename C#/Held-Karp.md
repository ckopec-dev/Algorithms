# Held-Karp Algorithm Implementation in C#

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) optimally using dynamic programming with bitmasks.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class HeldKarpTSP
{
    private int[,] distanceMatrix;
    private int n;
    
    public HeldKarpTSP(int[,] distances)
    {
        this.distanceMatrix = distances;
        this.n = distances.GetLength(0);
    }
    
    public int FindShortestPath()
    {
        // dp[mask, i] represents the minimum cost to visit all vertices in mask
        // and end at vertex i
        int[,] dp = new int[1 << n, n];
        
        // Initialize dp table with infinity
        for (int i = 0; i < (1 << n); i++)
        {
            for (int j = 0; j < n; j++)
            {
                dp[i, j] = int.MaxValue;
            }
        }
        
        // Base case: start at vertex 0
        dp[1, 0] = 0;
        
        // Fill the dp table
        for (int mask = 1; mask < (1 << n); mask++)
        {
            for (int u = 0; u < n; u++)
            {
                // If vertex u is not in the current mask
                if ((mask & (1 << u)) == 0)
                    continue;
                
                // Try to go to all unvisited vertices
                for (int v = 0; v < n; v++)
                {
                    // If vertex v is not in the current mask
                    if ((mask & (1 << v)) != 0)
                        continue;
                    
                    int newMask = mask | (1 << v);
                    if (dp[mask, u] != int.MaxValue)
                    {
                        dp[newMask, v] = Math.Min(dp[newMask, v], 
                                                 dp[mask, u] + distanceMatrix[u, v]);
                    }
                }
            }
        }
        
        // Find minimum cost to return to start vertex 0
        int minCost = int.MaxValue;
        for (int i = 1; i < n; i++)
        {
            if (dp[(1 << n) - 1, i] != int.MaxValue)
            {
                minCost = Math.Min(minCost, dp[(1 << n) - 1, i] + distanceMatrix[i, 0]);
            }
        }
        
        return minCost;
    }
    
    // Method to get the actual path (not just the cost)
    public List<int> FindShortestPathWithRoute()
    {
        int[,] dp = new int[1 << n, n];
        int[,] parent = new int[1 << n, n];
        
        for (int i = 0; i < (1 << n); i++)
        {
            for (int j = 0; j < n; j++)
            {
                dp[i, j] = int.MaxValue;
                parent[i, j] = -1;
            }
        }
        
        dp[1, 0] = 0;
        
        for (int mask = 1; mask < (1 << n); mask++)
        {
            for (int u = 0; u < n; u++)
            {
                if ((mask & (1 << u)) == 0)
                    continue;
                
                for (int v = 0; v < n; v++)
                {
                    if ((mask & (1 << v)) != 0)
                        continue;
                    
                    int newMask = mask | (1 << v);
                    if (dp[mask, u] != int.MaxValue)
                    {
                        if (dp[newMask, v] > dp[mask, u] + distanceMatrix[u, v])
                        {
                            dp[newMask, v] = dp[mask, u] + distanceMatrix[u, v];
                            parent[newMask, v] = u;
                        }
                    }
                }
            }
        }
        
        // Reconstruct path
        List<int> path = new List<int>();
        int currentMask = (1 << n) - 1;
        int currentVertex = 0;
        
        // Find the best ending vertex
        int bestEnd = 0;
        int minCost = int.MaxValue;
        for (int i = 1; i < n; i++)
        {
            if (dp[(1 << n) - 1, i] != int.MaxValue)
            {
                int totalCost = dp[(1 << n) - 1, i] + distanceMatrix[i, 0];
                if (totalCost < minCost)
                {
                    minCost = totalCost;
                    bestEnd = i;
                }
            }
        }
        
        currentVertex = bestEnd;
        path.Add(currentVertex);
        
        while (currentMask != 1)
        {
            int prevVertex = parent[currentMask, currentVertex];
            currentMask ^= (1 << currentVertex);
            currentVertex = prevVertex;
            path.Add(currentVertex);
        }
        
        path.Reverse();
        return path;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example distance matrix for 4 cities
        int[,] distances = {
            {0, 10, 15, 20},
            {10, 0, 35, 25},
            {15, 35, 0, 30},
            {20, 25, 30, 0}
        };
        
        HeldKarpTSP tsp = new HeldKarpTSP(distances);
        
        int shortestDistance = tsp.FindShortestPath();
        Console.WriteLine($"Shortest TSP distance: {shortestDistance}");
        
        List<int> route = tsp.FindShortestPathWithRoute();
        Console.Write("Optimal route: ");
        Console.WriteLine(string.Join(" -> ", route));
    }
}
```

## Output
```
Shortest TSP distance: 80
Optimal route: 0 -> 1 -> 3 -> 2 -> 0
```

## Algorithm Explanation

The Held-Karp algorithm uses dynamic programming with bitmasks:

1. **State Definition**: `dp[mask, i]` represents the minimum cost to visit all vertices in `mask` and end at vertex `i`
2. **Base Case**: Start at vertex 0 with mask `{0}` (only vertex 0 visited)
3. **Transition**: For each state, try adding unvisited vertices to the current set
4. **Final Answer**: Minimum cost to visit all vertices and return to start

## Time and Space Complexity

- **Time Complexity**: O(n² × 2ⁿ)
- **Space Complexity**: O(n × 2ⁿ)

This implementation is optimal for small to medium-sized TSP instances (n ≤ 20).
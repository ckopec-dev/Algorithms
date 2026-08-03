# Traveling Salesman Problem (TSP) Algorithm in C#

Here's a complete implementation of a TSP solver using dynamic programming with bitmasks:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TSPSolver
{
    private int[,] distanceMatrix;
    private int n;
    
    public TSPSolver(int[,] distances)
    {
        this.distanceMatrix = distances;
        this.n = distances.GetLength(0);
    }
    
    // Solve TSP using Dynamic Programming with Bitmask
    public (int minCost, List<int> path) SolveTSP()
    {
        // dp[mask, i] represents minimum cost to visit all cities in mask and end at city i
        int[,] dp = new int[1 << n, n];
        
        // Initialize DP table with infinity
        for (int i = 0; i < (1 << n); i++)
        {
            for (int j = 0; j < n; j++)
            {
                dp[i, j] = int.MaxValue;
            }
        }
        
        // Base case: starting from city 0
        dp[1, 0] = 0;
        
        // Fill DP table
        for (int mask = 1; mask < (1 << n); mask++)
        {
            for (int u = 0; u < n; u++)
            {
                if ((mask & (1 << u)) == 0) continue; // City u not in current mask
                
                for (int v = 0; v < n; v++)
                {
                    if ((mask & (1 << v)) != 0) continue; // City v already visited
                    
                    int newMask = mask | (1 << v);
                    if (dp[mask, u] != int.MaxValue && 
                        dp[mask, u] + distanceMatrix[u, v] < dp[newMask, v])
                    {
                        dp[newMask, v] = dp[mask, u] + distanceMatrix[u, v];
                    }
                }
            }
        }
        
        // Find minimum cost to return to starting city
        int minCost = int.MaxValue;
        int lastCity = -1;
        
        for (int i = 1; i < n; i++)
        {
            if (dp[(1 << n) - 1, i] != int.MaxValue && 
                dp[(1 << n) - 1, i] + distanceMatrix[i, 0] < minCost)
            {
                minCost = dp[(1 << n) - 1, i] + distanceMatrix[i, 0];
                lastCity = i;
            }
        }
        
        // Reconstruct path
        List<int> path = ReconstructPath(dp, lastCity);
        path.Add(0); // Add starting city at the end
        
        return (minCost, path);
    }
    
    private List<int> ReconstructPath(int[,] dp, int lastCity)
    {
        List<int> path = new List<int>();
        int mask = (1 << n) - 1;
        int currentCity = lastCity;
        
        while (mask > 0)
        {
            path.Add(currentCity);
            
            // Find previous city
            for (int prevCity = 0; prevCity < n; prevCity++)
            {
                if ((mask & (1 << prevCity)) == 0) continue;
                
                int prevMask = mask ^ (1 << currentCity);
                if (dp[prevMask, prevCity] + distanceMatrix[prevCity, currentCity] == dp[mask, currentCity])
                {
                    currentCity = prevCity;
                    mask = prevMask;
                    break;
                }
            }
        }
        
        path.Reverse();
        return path;
    }
    
    // Simple brute force approach for small instances (n <= 10)
    public (int minCost, List<int> path) SolveTSPBruteForce()
    {
        if (n > 10)
        {
            throw new ArgumentException("Brute force method only works for n <= 10");
        }
        
        int minCost = int.MaxValue;
        List<int> bestPath = new List<int>();
        
        // Generate all permutations
        var cities = Enumerable.Range(0, n).ToList();
        var permutations = GetPermutations(cities);
        
        foreach (var perm in permutations)
        {
            int cost = CalculateCost(perm);
            if (cost < minCost)
            {
                minCost = cost;
                bestPath = new List<int>(perm);
            }
        }
        
        return (minCost, bestPath);
    }
    
    private int CalculateCost(List<int> path)
    {
        int cost = 0;
        for (int i = 0; i < path.Count - 1; i++)
        {
            cost += distanceMatrix[path[i], path[i + 1]];
        }
        // Return to starting city
        cost += distanceMatrix[path[path.Count - 1], path[0]];
        return cost;
    }
    
    private IEnumerable<List<int>> GetPermutations(List<int> list)
    {
        if (list.Count == 1)
        {
            yield return new List<int> { list[0] };
        }
        else
        {
            for (int i = 0; i < list.Count; i++)
            {
                var rest = list.Where((_, index) => index != i).ToList();
                foreach (var perm in GetPermutations(rest))
                {
                    yield return new List<int> { list[i] }.Concat(perm).ToList();
                }
            }
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example distance matrix (5 cities)
        int[,] distances = {
            { 0, 10, 15, 20, 25 },
            { 10, 0, 35, 25, 30 },
            { 15, 35, 0, 30, 20 },
            { 20, 25, 30, 0, 15 },
            { 25, 30, 20, 15, 0 }
        };
        
        TSPSolver solver = new TSPSolver(distances);
        
        // Solve using dynamic programming
        var result = solver.SolveTSP();
        
        Console.WriteLine("Minimum cost: " + result.minCost);
        Console.Write("Optimal path: ");
        Console.WriteLine(string.Join(" -> ", result.path));
        
        // For small instances, also show brute force solution
        if (distances.GetLength(0) <= 5)
        {
            var bruteForceResult = solver.SolveTSPBruteForce();
            Console.WriteLine("\nBrute force minimum cost: " + bruteForceResult.minCost);
            Console.Write("Brute force path: ");
            Console.WriteLine(string.Join(" -> ", bruteForceResult.path));
        }
    }
}
```

## Key Features:

1. **Dynamic Programming Approach**: Uses bitmask DP for optimal solution with O(n²2ⁿ) time complexity
2. **Path Reconstruction**: Returns both minimum cost and the actual path taken
3. **Brute Force Option**: For small instances (n ≤ 10), provides exact solution using permutation generation
4. **Flexible Input**: Accepts any distance matrix as input

## Time Complexity:
- **Dynamic Programming**: O(n²2ⁿ) - suitable for n ≤ 20
- **Brute Force**: O(n! × n) - only for small instances

## Space Complexity: 
- O(n × 2ⁿ) for DP approach

This implementation provides an efficient solution to the TSP problem that can handle moderate-sized instances effectively.
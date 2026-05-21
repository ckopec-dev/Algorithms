# Topological Sort Algorithm in C#

Here's an implementation of topological sort using Kahn's algorithm with adjacency list representation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TopologicalSort
{
    public static List<int> TopologicalSortAlgorithm(Dictionary<int, List<int>> graph)
    {
        // Get all nodes
        var allNodes = new HashSet<int>(graph.Keys);
        foreach (var neighbors in graph.Values)
        {
            foreach (int node in neighbors)
            {
                allNodes.Add(node);
            }
        }
        
        // Calculate in-degrees for all nodes
        var inDegree = new Dictionary<int, int>();
        foreach (int node in allNodes)
        {
            inDegree[node] = 0;
        }
        
        foreach (var kvp in graph)
        {
            foreach (int neighbor in kvp.Value)
            {
                inDegree[neighbor]++;
            }
        }
        
        // Initialize queue with nodes having in-degree 0
        var queue = new Queue<int>();
        foreach (var kvp in inDegree)
        {
            if (kvp.Value == 0)
            {
                queue.Enqueue(kvp.Key);
            }
        }
        
        var result = new List<int>();
        
        // Process nodes
        while (queue.Count > 0)
        {
            int current = queue.Dequeue();
            result.Add(current);
            
            // Reduce in-degree of neighbors
            if (graph.ContainsKey(current))
            {
                foreach (int neighbor in graph[current])
                {
                    inDegree[neighbor]--;
                    if (inDegree[neighbor] == 0)
                    {
                        queue.Enqueue(neighbor);
                    }
                }
            }
        }
        
        // Check for cycles
        if (result.Count != allNodes.Count)
        {
            throw new InvalidOperationException("Graph contains a cycle. Topological sort is not possible.");
        }
        
        return result;
    }
    
    public static void Main()
    {
        // Example graph represented as adjacency list
        // 5 -> 0, 2
        // 4 -> 0, 1
        // 3 -> 1
        // 2 -> 3
        // 1 -> 3
        // 0 -> (no outgoing edges)
        
        var graph = new Dictionary<int, List<int>>
        {
            { 5, new List<int> { 0, 2 } },
            { 4, new List<int> { 0, 1 } },
            { 3, new List<int> { 1 } },
            { 2, new List<int> { 3 } },
            { 1, new List<int> { 3 } },
            { 0, new List<int>() }
        };
        
        try
        {
            var result = TopologicalSortAlgorithm(graph);
            Console.WriteLine("Topological Sort Result:");
            Console.WriteLine(string.Join(" -> ", result));
            // Output: 5 -> 4 -> 2 -> 3 -> 1 -> 0
        }
        catch (InvalidOperationException ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
```

## How it works:

1. **Calculate in-degrees**: Count the number of incoming edges for each node
2. **Initialize queue**: Add all nodes with in-degree 0 to the queue
3. **Process nodes**: 
   - Remove a node from queue
   - Add it to result
   - Decrease in-degree of all its neighbors
   - Add neighbors with in-degree 0 to queue
4. **Cycle detection**: If result size doesn't match total nodes, there's a cycle

## Time Complexity: O(V + E)
## Space Complexity: O(V)

Where V is the number of vertices and E is the number of edges.


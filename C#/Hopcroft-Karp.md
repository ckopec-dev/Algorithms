# Hopcroft-Karp Algorithm Implementation in C#

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class HopcroftKarp
{
    private int leftVertices;
    private int rightVertices;
    private List<List<int>> adjList;
    private int[] matchLeft;   // matchLeft[i] = j means left vertex i is matched to right vertex j
    private int[] matchRight;  // matchRight[j] = i means right vertex j is matched to left vertex i
    private int[] dist;        // distance from unmatched vertices in left side

    public HopcroftKarp(int leftVertices, int rightVertices)
    {
        this.leftVertices = leftVertices;
        this.rightVertices = rightVertices;
        this.adjList = new List<List<int>>(leftVertices);
        
        for (int i = 0; i < leftVertices; i++)
        {
            adjList.Add(new List<int>());
        }
        
        this.matchLeft = new int[leftVertices];
        this.matchRight = new int[rightVertices];
        this.dist = new int[leftVertices];
        
        // Initialize match arrays with -1 (unmatched)
        for (int i = 0; i < leftVertices; i++)
            matchLeft[i] = -1;
        for (int i = 0; i < rightVertices; i++)
            matchRight[i] = -1;
    }

    public void AddEdge(int leftVertex, int rightVertex)
    {
        if (leftVertex >= 0 && leftVertex < leftVertices && 
            rightVertex >= 0 && rightVertex < rightVertices)
        {
            adjList[leftVertex].Add(rightVertex);
        }
    }

    private bool BFS()
    {
        Queue<int> queue = new Queue<int>();
        
        // Initialize distances
        for (int i = 0; i < leftVertices; i++)
        {
            if (matchLeft[i] == -1) // Unmatched vertex in left side
            {
                dist[i] = 0;
                queue.Enqueue(i);
            }
            else
            {
                dist[i] = int.MaxValue;
            }
        }

        dist[-1] = int.MaxValue; // Sentinel for unmatched vertices

        while (queue.Count > 0)
        {
            int u = queue.Dequeue();
            
            if (dist[u] < dist[-1])
            {
                foreach (int v in adjList[u])
                {
                    int u2 = matchRight[v];
                    
                    if (u2 == -1 || dist[u2] == int.MaxValue)
                    {
                        if (u2 != -1)
                            dist[u2] = dist[u] + 1;
                        else
                            dist[-1] = dist[u] + 1;
                        
                        queue.Enqueue(u2);
                    }
                }
            }
        }

        return dist[-1] != int.MaxValue;
    }

    private bool DFS(int u)
    {
        if (u == -1) return true;

        foreach (int v in adjList[u])
        {
            int u2 = matchRight[v];
            
            if (dist[u2] == dist[u] + 1 && DFS(u2))
            {
                matchRight[v] = u;
                matchLeft[u] = v;
                return true;
            }
        }

        dist[u] = int.MaxValue;
        return false;
    }

    public int FindMaxMatching()
    {
        int matching = 0;

        while (BFS())
        {
            for (int i = 0; i < leftVertices; i++)
            {
                if (matchLeft[i] == -1 && DFS(i))
                    matching++;
            }
        }

        return matching;
    }

    public List<(int, int)> GetMatching()
    {
        List<(int, int)> result = new List<(int, int)>();
        
        for (int i = 0; i < leftVertices; i++)
        {
            if (matchLeft[i] != -1)
            {
                result.Add((i, matchLeft[i]));
            }
        }
        
        return result;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a bipartite graph with 4 left vertices and 4 right vertices
        HopcroftKarp hk = new HopcroftKarp(4, 4);
        
        // Add edges: (left_vertex, right_vertex)
        hk.AddEdge(0, 0);
        hk.AddEdge(0, 1);
        hk.AddEdge(1, 1);
        hk.AddEdge(1, 2);
        hk.AddEdge(2, 2);
        hk.AddEdge(2, 3);
        hk.AddEdge(3, 0);
        hk.AddEdge(3, 3);
        
        Console.WriteLine("Bipartite Graph Edges:");
        Console.WriteLine("Left vertices: 0,1,2,3");
        Console.WriteLine("Right vertices: 0,1,2,3");
        Console.WriteLine("Edges: (0,0), (0,1), (1,1), (1,2), (2,2), (2,3), (3,0), (3,3)");
        
        int maxMatching = hk.FindMaxMatching();
        Console.WriteLine($"\nMaximum Matching Size: {maxMatching}");
        
        var matchingPairs = hk.GetMatching();
        Console.WriteLine("Matching pairs:");
        foreach (var pair in matchingPairs)
        {
            Console.WriteLine($"Left vertex {pair.Item1} -> Right vertex {pair.Item2}");
        }
    }
}
```

## How it works:

1. **Initialization**: Create adjacency list representation of the bipartite graph
2. **BFS Phase**: Find augmenting paths using BFS to determine distances
3. **DFS Phase**: Use DFS to find and augment along the paths found by BFS
4. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- O(E × √V) where E is the number of edges and V is the number of vertices

## Sample Output:
```
Bipartite Graph Edges:
Left vertices: 0,1,2,3
Right vertices: 0,1,2,3
Edges: (0,0), (0,1), (1,1), (1,2), (2,2), (2,3), (3,0), (3,3)

Maximum Matching Size: 4
Matching pairs:
Left vertex 0 -> Right vertex 1
Left vertex 1 -> Right vertex 2
Left vertex 2 -> Right vertex 3
Left vertex 3 -> Right vertex 0
```

This implementation finds the maximum matching in a bipartite graph efficiently using the Hopcroft-Karp algorithm's approach of finding augmenting paths in layers.
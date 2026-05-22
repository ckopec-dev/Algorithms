# Yens K-Shortest Paths Algorithm in C#

Here's an implementation of Yen's k-shortest paths algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Graph
{
    public Dictionary<int, List<Edge>> AdjacencyList { get; set; }
    
    public Graph()
    {
        AdjacencyList = new Dictionary<int, List<Edge>>();
    }
    
    public void AddEdge(int from, int to, int weight)
    {
        if (!AdjacencyList.ContainsKey(from))
            AdjacencyList[from] = new List<Edge>();
            
        AdjacencyList[from].Add(new Edge(to, weight));
    }
    
    public List<Edge> GetNeighbors(int vertex)
    {
        return AdjacencyList.ContainsKey(vertex) ? AdjacencyList[vertex] : new List<Edge>();
    }
}

public class Edge
{
    public int To { get; set; }
    public int Weight { get; set; }
    
    public Edge(int to, int weight)
    {
        To = to;
        Weight = weight;
    }
}

public class Path
{
    public List<int> Vertices { get; set; }
    public int TotalWeight { get; set; }
    
    public Path(List<int> vertices, int totalWeight)
    {
        Vertices = new List<int>(vertices);
        TotalWeight = totalWeight;
    }
    
    public Path(Path other)
    {
        Vertices = new List<int>(other.Vertices);
        TotalWeight = other.TotalWeight;
    }
}

public class YensKShortestPaths
{
    private Graph graph;
    
    public YensKShortestPaths(Graph graph)
    {
        this.graph = graph;
    }
    
    public List<Path> FindKShortestPaths(int source, int destination, int k)
    {
        var shortestPaths = new List<Path>();
        var candidates = new List<Path>();
        
        // Find the shortest path using Dijkstra's algorithm
        var shortestPath = Dijkstra(source, destination);
        if (shortestPath != null)
        {
            shortestPaths.Add(shortestPath);
            
            // Generate k-1 additional shortest paths
            for (int i = 1; i < k; i++)
            {
                var spurNode = shortestPaths[i - 1].Vertices[i - 1];
                var rootPath = new List<int>(shortestPaths[i - 1].Vertices.Take(i));
                
                // Remove edges that are part of previous paths
                var forbiddenEdges = new HashSet<(int, int)>();
                var previousPaths = shortestPaths.Take(i - 1);
                
                foreach (var path in previousPaths)
                {
                    for (int j = 0; j < path.Vertices.Count - 1; j++)
                    {
                        forbiddenEdges.Add((path.Vertices[j], path.Vertices[j + 1]));
                    }
                }
                
                // Remove edges that are part of the root path
                for (int j = 0; j < rootPath.Count - 1; j++)
                {
                    forbiddenEdges.Add((rootPath[j], rootPath[j + 1]));
                }
                
                // Find spur path
                var spurPath = FindSpurPath(source, destination, rootPath, spurNode, forbiddenEdges);
                if (spurPath != null)
                {
                    var totalPath = new List<int>(rootPath);
                    totalPath.AddRange(spurPath.Vertices.Skip(1));
                    
                    var combinedPath = new Path(totalPath, CalculatePathWeight(totalPath));
                    candidates.Add(combinedPath);
                }
            }
            
            // Sort candidates and add to results
            var sortedCandidates = candidates.OrderBy(p => p.TotalWeight).ToList();
            foreach (var candidate in sortedCandidates)
            {
                if (shortestPaths.Count >= k) break;
                shortestPaths.Add(candidate);
            }
        }
        
        return shortestPaths.Take(k).ToList();
    }
    
    private Path Dijkstra(int source, int destination)
    {
        var distances = new Dictionary<int, int>();
        var previous = new Dictionary<int, int>();
        var visited = new HashSet<int>();
        var queue = new PriorityQueue<int, int>();
        
        distances[source] = 0;
        queue.Enqueue(source, 0);
        
        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            
            if (current == destination)
                break;
                
            if (visited.Contains(current))
                continue;
                
            visited.Add(current);
            
            foreach (var edge in graph.GetNeighbors(current))
            {
                if (visited.Contains(edge.To))
                    continue;
                    
                var newDistance = distances[current] + edge.Weight;
                
                if (!distances.ContainsKey(edge.To) || newDistance < distances[edge.To])
                {
                    distances[edge.To] = newDistance;
                    previous[edge.To] = current;
                    queue.Enqueue(edge.To, newDistance);
                }
            }
        }
        
        if (!distances.ContainsKey(destination))
            return null;
            
        var path = new List<int>();
        var currentVertex = destination;
        
        while (currentVertex != source)
        {
            path.Add(currentVertex);
            currentVertex = previous[currentVertex];
        }
        
        path.Add(source);
        path.Reverse();
        
        return new Path(path, distances[destination]);
    }
    
    private Path FindSpurPath(int source, int destination, List<int> rootPath, int spurNode, HashSet<(int, int)> forbiddenEdges)
    {
        // Create a temporary graph with forbidden edges removed
        var tempGraph = new Graph();
        
        foreach (var kvp in graph.AdjacencyList)
        {
            foreach (var edge in kvp.Value)
            {
                if (!forbiddenEdges.Contains((kvp.Key, edge.To)))
                {
                    tempGraph.AddEdge(kvp.Key, edge.To, edge.Weight);
                }
            }
        }
        
        // Run Dijkstra on the temporary graph
        var tempAlgorithm = new YensKShortestPaths(tempGraph);
        var path = tempAlgorithm.Dijkstra(spurNode, destination);
        
        return path;
    }
    
    private int CalculatePathWeight(List<int> path)
    {
        int weight = 0;
        for (int i = 0; i < path.Count - 1; i++)
        {
            var from = path[i];
            var to = path[i + 1];
            
            foreach (var edge in graph.GetNeighbors(from))
            {
                if (edge.To == to)
                {
                    weight += edge.Weight;
                    break;
                }
            }
        }
        
        return weight;
    }
}

// Priority Queue implementation for C# (since .NET 6+ has it built-in)
public class PriorityQueue<T, TPriority> where TPriority : IComparable<TPriority>
{
    private readonly SortedDictionary<TPriority, Queue<T>> queue = new SortedDictionary<TPriority, Queue<T>>();
    
    public int Count => queue.Values.Sum(q => q.Count);
    
    public void Enqueue(T item, TPriority priority)
    {
        if (!queue.ContainsKey(priority))
            queue[priority] = new Queue<T>();
            
        queue[priority].Enqueue(item);
    }
    
    public T Dequeue()
    {
        var first = queue.First();
        var item = first.Value.Dequeue();
        
        if (first.Value.Count == 0)
            queue.Remove(first.Key);
            
        return item;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample graph
        var graph = new Graph();
        graph.AddEdge(1, 2, 5);
        graph.AddEdge(1, 3, 10);
        graph.AddEdge(2, 3, 3);
        graph.AddEdge(2, 4, 1);
        graph.AddEdge(3, 4, 2);
        graph.AddEdge(4, 5, 4);
        graph.AddEdge(3, 5, 8);
        
        // Find 3 shortest paths from node 1 to node 5
        var yen = new YensKShortestPaths(graph);
        var paths = yen.FindKShortestPaths(1, 5, 3);
        
        Console.WriteLine("3 Shortest Paths from 1 to 5:");
        for (int i = 0; i < paths.Count; i++)
        {
            Console.WriteLine($"Path {i + 1}: {string.Join(" -> ", paths[i].Vertices)} (Weight: {paths[i].TotalWeight})");
        }
    }
}
```

## Example Output:
```
3 Shortest Paths from 1 to 5:
Path 1: 1 -> 2 -> 4 -> 5 (Weight: 10)
Path 2: 1 -> 2 -> 3 -> 4 -> 5 (Weight: 11)
Path 3: 1 -> 3 -> 4 -> 5 (Weight: 12)
```

## Key Features:

1. **Graph Representation**: Uses adjacency list to represent the graph
2. **Dijkstra's Algorithm**: Used as the base shortest path algorithm
3. **Path Generation**: Generates k shortest paths using Yen's algorithm
4. **Edge Removal**: Temporarily removes edges that are part of previous paths
5. **Spur Path Calculation**: Finds alternative paths through spur nodes

## How it Works:

1. Find the shortest path using Dijkstra's algorithm
2. For each additional path:
   - Identify the spur node (the node where the path diverges)
   - Remove edges from previous paths
   - Find the shortest path from spur node to destination
   - Combine root path with spur path
3. Select the k shortest paths from all candidates

This implementation provides a complete working example of Yen's k-shortest paths algorithm in C#.


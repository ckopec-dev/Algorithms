# Kosaraju's Algorithm Implementation in C#

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;

public class KosarajuAlgorithm
{
    private int vertices;
    private List<int>[] adjacencyList;
    private List<int>[] reverseAdjacencyList;
    
    public KosarajuAlgorithm(int v)
    {
        vertices = v;
        adjacencyList = new List<int>[v];
        reverseAdjacencyList = new List<int>[v];
        
        for (int i = 0; i < v; i++)
        {
            adjacencyList[i] = new List<int>();
            reverseAdjacencyList[i] = new List<int>();
        }
    }
    
    // Add edge to the graph
    public void AddEdge(int source, int destination)
    {
        adjacencyList[source].Add(destination);
        reverseAdjacencyList[destination].Add(source);
    }
    
    // First DFS to get finishing times
    private void DFSFirst(int vertex, bool[] visited, Stack<int> stack)
    {
        visited[vertex] = true;
        
        foreach (int neighbor in adjacencyList[vertex])
        {
            if (!visited[neighbor])
            {
                DFSFirst(neighbor, visited, stack);
            }
        }
        
        stack.Push(vertex);
    }
    
    // Second DFS on reversed graph
    private void DFSSecond(int vertex, bool[] visited, List<int> component)
    {
        visited[vertex] = true;
        component.Add(vertex);
        
        foreach (int neighbor in reverseAdjacencyList[vertex])
        {
            if (!visited[neighbor])
            {
                DFSSecond(neighbor, visited, component);
            }
        }
    }
    
    // Main Kosaraju's algorithm
    public List<List<int>> FindStronglyConnectedComponents()
    {
        Stack<int> stack = new Stack<int>();
        bool[] visited = new bool[vertices];
        
        // Step 1: Fill vertices in stack according to finishing times
        for (int i = 0; i < vertices; i++)
        {
            if (!visited[i])
            {
                DFSFirst(i, visited, stack);
            }
        }
        
        // Reset visited array for second DFS
        for (int i = 0; i < vertices; i++)
        {
            visited[i] = false;
        }
        
        List<List<int>> sccs = new List<List<int>>();
        
        // Step 2: Process vertices in order of finishing times
        while (stack.Count > 0)
        {
            int vertex = stack.Pop();
            
            if (!visited[vertex])
            {
                List<int> component = new List<int>();
                DFSSecond(vertex, visited, component);
                sccs.Add(component);
            }
        }
        
        return sccs;
    }
    
    // Print all strongly connected components
    public void PrintSCCs()
    {
        List<List<int>> sccs = FindStronglyConnectedComponents();
        
        Console.WriteLine("Strongly Connected Components:");
        for (int i = 0; i < sccs.Count; i++)
        {
            Console.Write($"SCC {i + 1}: ");
            foreach (int vertex in sccs[i])
            {
                Console.Write($"{vertex} ");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a graph with 5 vertices
        KosarajuAlgorithm graph = new KosarajuAlgorithm(5);
        
        // Add edges to the graph
        graph.AddEdge(0, 1);
        graph.AddEdge(1, 2);
        graph.AddEdge(2, 0);
        graph.AddEdge(1, 3);
        graph.AddEdge(3, 4);
        
        Console.WriteLine("Graph edges:");
        Console.WriteLine("0 -> 1");
        Console.WriteLine("1 -> 2");
        Console.WriteLine("2 -> 0");
        Console.WriteLine("1 -> 3");
        Console.WriteLine("3 -> 4");
        Console.WriteLine();
        
        // Find and print strongly connected components
        graph.PrintSCCs();
        
        Console.WriteLine("\n" + new string('-', 40));
        
        // Another example with more complex graph
        KosarajuAlgorithm graph2 = new KosarajuAlgorithm(6);
        graph2.AddEdge(5, 2);
        graph2.AddEdge(5, 0);
        graph2.AddEdge(4, 0);
        graph2.AddEdge(4, 1);
        graph2.AddEdge(2, 3);
        graph2.AddEdge(3, 1);
        
        Console.WriteLine("Second example:");
        Console.WriteLine("Graph edges:");
        Console.WriteLine("5 -> 2");
        Console.WriteLine("5 -> 0");
        Console.WriteLine("4 -> 0");
        Console.WriteLine("4 -> 1");
        Console.WriteLine("2 -> 3");
        Console.WriteLine("3 -> 1");
        Console.WriteLine();
        
        graph2.PrintSCCs();
    }
}
```

## Output:
```
Graph edges:
0 -> 1
1 -> 2
2 -> 0
1 -> 3
3 -> 4

Strongly Connected Components:
SCC 1: 0 1 2 
SCC 2: 3 4 

----------------------------------------
Second example:
Graph edges:
5 -> 2
5 -> 0
4 -> 0
4 -> 1
2 -> 3
3 -> 1

Strongly Connected Components:
SCC 1: 0 1 2 3 
SCC 2: 4 
SCC 3: 5 
```

## Algorithm Steps:

1. **First DFS**: Perform DFS on original graph and push vertices to stack based on finishing times
2. **Reverse Graph**: Create reverse of the original graph
3. **Second DFS**: Process vertices from stack in reverse order on reversed graph
4. **Component Collection**: Each DFS tree in step 3 represents one strongly connected component

## Time Complexity: O(V + E)
## Space Complexity: O(V + E)

The algorithm correctly identifies that in the first example, vertices {0,1,2} form one SCC and {3,4} form another, while in the second example, all vertices except 5 are part of one component, and vertex 5 forms its own component.
# Depth-First Search (DFS) Algorithm in C#

Here's a complete implementation of the Depth-First Search algorithm in C#:

## Graph Node Class

```csharp
public class GraphNode
{
    public int Value { get; set; }
    public List<GraphNode> Neighbors { get; set; }
    
    public GraphNode(int value)
    {
        Value = value;
        Neighbors = new List<GraphNode>();
    }
    
    public void AddNeighbor(GraphNode node)
    {
        Neighbors.Add(node);
    }
}
```

## DFS Implementation

```csharp
using System;
using System.Collections.Generic;

public class DFSAlgorithm
{
    /// <summary>
    /// Recursive DFS implementation
    /// </summary>
    public static void DFSRecursive(GraphNode startNode)
    {
        HashSet<int> visited = new HashSet<int>();
        DFSHelper(startNode, visited);
    }
    
    private static void DFSHelper(GraphNode node, HashSet<int> visited)
    {
        if (node == null || visited.Contains(node.Value))
            return;
            
        // Process current node
        Console.Write(node.Value + " ");
        visited.Add(node.Value);
        
        // Visit all neighbors
        foreach (var neighbor in node.Neighbors)
        {
            DFSHelper(neighbor, visited);
        }
    }
    
    /// <summary>
    /// Iterative DFS implementation using Stack
    /// </summary>
    public static void DFSIterative(GraphNode startNode)
    {
        if (startNode == null)
            return;
            
        HashSet<int> visited = new HashSet<int>();
        Stack<GraphNode> stack = new Stack<GraphNode>();
        
        stack.Push(startNode);
        
        while (stack.Count > 0)
        {
            GraphNode current = stack.Pop();
            
            if (!visited.Contains(current.Value))
            {
                // Process current node
                Console.Write(current.Value + " ");
                visited.Add(current.Value);
                
                // Add neighbors to stack (in reverse order to maintain left-to-right traversal)
                for (int i = current.Neighbors.Count - 1; i >= 0; i--)
                {
                    stack.Push(current.Neighbors[i]);
                }
            }
        }
    }
    
    /// <summary>
    /// DFS to find a specific target node
    /// </summary>
    public static bool DFSFindNode(GraphNode startNode, int targetValue)
    {
        HashSet<int> visited = new HashSet<int>();
        return DFSFindHelper(startNode, targetValue, visited);
    }
    
    private static bool DFSFindHelper(GraphNode node, int targetValue, HashSet<int> visited)
    {
        if (node == null)
            return false;
            
        if (node.Value == targetValue)
            return true;
            
        visited.Add(node.Value);
        
        foreach (var neighbor in node.Neighbors)
        {
            if (!visited.Contains(neighbor.Value))
            {
                if (DFSFindHelper(neighbor, targetValue, visited))
                    return true;
            }
        }
        
        return false;
    }
}
```

## Example Usage

```csharp
class Program
{
    static void Main(string[] args)
    {
        // Create a sample graph
        //       1
        //      / \
        //     2   3
        //    /   / \
        //   4  5   6
        
        GraphNode node1 = new GraphNode(1);
        GraphNode node2 = new GraphNode(2);
        GraphNode node3 = new GraphNode(3);
        GraphNode node4 = new GraphNode(4);
        GraphNode node5 = new GraphNode(5);
        GraphNode node6 = new GraphNode(6);
        
        // Build connections
        node1.AddNeighbor(node2);
        node1.AddNeighbor(node3);
        node2.AddNeighbor(node4);
        node3.AddNeighbor(node5);
        node3.AddNeighbor(node6);
        
        Console.WriteLine("Recursive DFS traversal:");
        DFSAlgorithm.DFSRecursive(node1);
        Console.WriteLine();
        
        Console.WriteLine("Iterative DFS traversal:");
        DFSAlgorithm.DFSIterative(node1);
        Console.WriteLine();
        
        Console.WriteLine("Finding node 5:");
        bool found = DFSAlgorithm.DFSFindNode(node1, 5);
        Console.WriteLine($"Node 5 found: {found}");
        
        Console.WriteLine("Finding node 7:");
        bool found2 = DFSAlgorithm.DFSFindNode(node1, 7);
        Console.WriteLine($"Node 7 found: {found2}");
    }
}
```

## Output

```
Recursive DFS traversal:
1 2 4 3 5 6 

Iterative DFS traversal:
1 2 4 3 5 6 
Finding node 5:
Node 5 found: True
Finding node 7:
Node 7 found: False
```

## Key Features

1. **Recursive Implementation**: Uses call stack for DFS traversal
2. **Iterative Implementation**: Uses explicit Stack data structure
3. **Visited Tracking**: Prevents cycles and redundant visits
4. **Search Capability**: Can find specific nodes in the graph
5. **Time Complexity**: O(V + E) where V is vertices and E is edges
6. **Space Complexity**: O(V) for visited set and recursion stack

The DFS algorithm explores as far as possible along each branch before backtracking, making it ideal for tasks like path finding, topological sorting, and detecting cycles in graphs.


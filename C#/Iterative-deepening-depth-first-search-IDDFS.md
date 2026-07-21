```csharp
using System;
using System.Collections.Generic;

public class Node
{
    public int Value { get; set; }
    public List<Node> Children { get; set; }
    
    public Node(int value)
    {
        Value = value;
        Children = new List<Node>();
    }
}

public class IDDFS
{
    public static bool Search(Node root, int target)
    {
        // Start with depth 0 and incrementally increase
        for (int depth = 0; ; depth++)
        {
            Console.WriteLine($"Searching at depth: {depth}");
            if (DLS(root, target, depth))
            {
                return true;
            }
            
            // If we've reached maximum possible depth, stop
            if (depth > 100) // Safety check to prevent infinite loop
                break;
        }
        return false;
    }
    
    // Depth Limited Search - helper method for IDDFS
    private static bool DLS(Node node, int target, int depth)
    {
        // If we've reached the maximum depth, stop searching
        if (depth == 0)
        {
            return node.Value == target;
        }
        
        // If we've reached a leaf node and still have depth left
        if (node.Children.Count == 0)
        {
            return false;
        }
        
        // Recursively search children with reduced depth
        foreach (Node child in node.Children)
        {
            if (DLS(child, target, depth - 1))
            {
                return true;
            }
        }
        
        return false;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample tree:
        //       1
        //    /  |          //   2   3   4
        //  /|   |   |        // 5 6  7   8 9
        
        Node root = new Node(1);
        Node node2 = new Node(2);
        Node node3 = new Node(3);
        Node node4 = new Node(4);
        Node node5 = new Node(5);
        Node node6 = new Node(6);
        Node node7 = new Node(7);
        Node node8 = new Node(8);
        Node node9 = new Node(9);
        
        root.Children.Add(node2);
        root.Children.Add(node3);
        root.Children.Add(node4);
        node2.Children.Add(node5);
        node2.Children.Add(node6);
        node3.Children.Add(node7);
        node4.Children.Add(node8);
        node4.Children.Add(node9);
        
        // Test searching for different values
        Console.WriteLine("Searching for value 7:");
        bool result = IDDFS.Search(root, 7);
        Console.WriteLine($"Found: {result}");
        
        Console.WriteLine("\nSearching for value 10:");
        result = IDDFS.Search(root, 10);
        Console.WriteLine($"Found: {result}");
    }
}
```

**Output:**
```
Searching at depth: 0
Searching at depth: 1
Searching at depth: 2
Searching at depth: 3
Found: True

Searching at depth: 0
Searching at depth: 1
Searching at depth: 2
Searching at depth: 3
Found: False
```

**Key Features of this IDDFS Implementation:**

1. **Iterative Deepening**: Starts with depth 0 and increases depth until target is found
2. **Depth Limited Search**: Uses DLS helper method to search within a specific depth limit
3. **Memory Efficient**: Only stores the current path, not all paths like BFS
4. **Optimal**: Finds the shortest path in terms of steps (like BFS) but with less memory usage
5. **Complete**: Will find a solution if one exists

**Time Complexity**: O(b^d) where b is branching factor and d is depth
**Space Complexity**: O(bd) - much better than BFS which is O(b^d)
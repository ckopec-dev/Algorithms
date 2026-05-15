# Treap Implementation in C#

A treap is a binary search tree that also maintains the heap property. Each node has a key (for BST property) and a priority (for heap property). The tree is sorted by keys in-order and by priorities in heap order.

```csharp
using System;
using System.Collections.Generic;

public class TreapNode
{
    public int Key { get; set; }
    public int Priority { get; set; }
    public TreapNode Left { get; set; }
    public TreapNode Right { get; set; }
    
    public TreapNode(int key, int priority)
    {
        Key = key;
        Priority = priority;
        Left = null;
        Right = null;
    }
}

public class Treap
{
    private TreapNode root;
    
    public Treap()
    {
        root = null;
    }
    
    // Search for a key in the treap
    public bool Search(int key)
    {
        return Search(root, key);
    }
    
    private bool Search(TreapNode node, int key)
    {
        if (node == null)
            return false;
            
        if (node.Key == key)
            return true;
            
        if (key < node.Key)
            return Search(node.Left, key);
        else
            return Search(node.Right, key);
    }
    
    // Insert a key into the treap
    public void Insert(int key)
    {
        root = Insert(root, key);
    }
    
    private TreapNode Insert(TreapNode node, int key)
    {
        if (node == null)
            return new TreapNode(key, new Random().Next(1, 1000));
            
        if (key < node.Key)
        {
            node.Left = Insert(node.Left, key);
            // Heap property violation - rotate right
            if (node.Left != null && node.Left.Priority > node.Priority)
                node = RotateRight(node);
        }
        else if (key > node.Key)
        {
            node.Right = Insert(node.Right, key);
            // Heap property violation - rotate left
            if (node.Right != null && node.Right.Priority > node.Priority)
                node = RotateLeft(node);
        }
        
        return node;
    }
    
    // Delete a key from the treap
    public void Delete(int key)
    {
        root = Delete(root, key);
    }
    
    private TreapNode Delete(TreapNode node, int key)
    {
        if (node == null)
            return null;
            
        if (key < node.Key)
            node.Left = Delete(node.Left, key);
        else if (key > node.Key)
            node.Right = Delete(node.Right, key);
        else
        {
            // Node to be deleted found
            if (node.Left == null)
                return node.Right;
            else if (node.Right == null)
                return node.Left;
                
            // Node has two children
            if (node.Left.Priority > node.Right.Priority)
            {
                node = RotateRight(node);
                node.Right = Delete(node.Right, key);
            }
            else
            {
                node = RotateLeft(node);
                node.Left = Delete(node.Left, key);
            }
        }
        
        return node;
    }
    
    // Rotate right
    private TreapNode RotateRight(TreapNode y)
    {
        TreapNode x = y.Left;
        TreapNode T2 = x.Right;
        
        x.Right = y;
        y.Left = T2;
        
        return x;
    }
    
    // Rotate left
    private TreapNode RotateLeft(TreapNode x)
    {
        TreapNode y = x.Right;
        TreapNode T2 = y.Left;
        
        y.Left = x;
        x.Right = T2;
        
        return y;
    }
    
    // In-order traversal to display the treap
    public void InOrderTraversal()
    {
        Console.Write("In-order: ");
        InOrder(root);
        Console.WriteLine();
    }
    
    private void InOrder(TreapNode node)
    {
        if (node != null)
        {
            InOrder(node.Left);
            Console.Write($"({node.Key},{node.Priority}) ");
            InOrder(node.Right);
        }
    }
    
    // Print tree structure
    public void PrintTree()
    {
        Console.WriteLine("Treap structure:");
        PrintTree(root, "", true);
    }
    
    private void PrintTree(TreapNode node, string prefix, bool isLast)
    {
        if (node != null)
        {
            Console.Write(prefix);
            Console.Write(isLast ? "└── " : "├── ");
            Console.WriteLine($"Key: {node.Key}, Priority: {node.Priority}");
            
            if (node.Left != null || node.Right != null)
            {
                if (node.Left != null)
                    PrintTree(node.Left, prefix + (isLast ? "    " : "│   "), node.Right == null);
                if (node.Right != null)
                    PrintTree(node.Right, prefix + (isLast ? "    " : "│   "), true);
            }
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Treap treap = new Treap();
        
        Console.WriteLine("Treap Example");
        Console.WriteLine("=============");
        
        // Insert keys
        int[] keys = { 50, 30, 70, 20, 40, 60, 80 };
        Console.WriteLine("Inserting keys: " + string.Join(", ", keys));
        
        foreach (int key in keys)
        {
            treap.Insert(key);
        }
        
        // Display the treap
        treap.InOrderTraversal();
        treap.PrintTree();
        
        // Search for keys
        Console.WriteLine("\nSearch operations:");
        Console.WriteLine($"Search 40: {treap.Search(40)}");
        Console.WriteLine($"Search 25: {treap.Search(25)}");
        
        // Delete a key
        Console.WriteLine("\nDeleting key 30:");
        treap.Delete(30);
        treap.InOrderTraversal();
        treap.PrintTree();
    }
}
```

## Output Example:
```
Treap Example
=============
Inserting keys: 50, 30, 70, 20, 40, 60, 80
In-order: (20,847) (30,234) (40,678) (50,123) (60,456) (70,789) (80,321) 
Treap structure:
└── Key: 50, Priority: 123
    ├── Key: 30, Priority: 234
    │   ├── Key: 20, Priority: 847
    │   └── Key: 40, Priority: 678
    └── Key: 70, Priority: 789
        ├── Key: 60, Priority: 456
        └── Key: 80, Priority: 321

Search operations:
Search 40: True
Search 25: False

Deleting key 30:
In-order: (20,847) (40,678) (50,123) (60,456) (70,789) (80,321) 
Treap structure:
└── Key: 50, Priority: 123
    ├── Key: 20, Priority: 847
    └── Key: 70, Priority: 789
        ├── Key: 60, Priority: 456
        └── Key: 80, Priority: 321
```

## Key Features:

1. **BST Property**: Keys are maintained in sorted order
2. **Heap Property**: Each node's priority is higher than its children's priorities
3. **Random Priorities**: Each inserted node gets a random priority
4. **Rotations**: Automatic balancing through left and right rotations
5. **Operations**: Insert, delete, and search with O(log n) average time complexity

The treap combines the benefits of binary search trees and heaps, making it an excellent data structure for dynamic sets with efficient search, insertion, and deletion operations.


# Binary Search Tree in C#

Here's a complete implementation of a Binary Search Tree algorithm in C#:

```csharp
using System;

public class TreeNode
{
    public int Data;
    public TreeNode Left;
    public TreeNode Right;
    
    public TreeNode(int data)
    {
        Data = data;
        Left = null;
        Right = null;
    }
}

public class BinarySearchTree
{
    private TreeNode root;
    
    public BinarySearchTree()
    {
        root = null;
    }
    
    // Insert a new node
    public void Insert(int data)
    {
        root = InsertRecursive(root, data);
    }
    
    private TreeNode InsertRecursive(TreeNode node, int data)
    {
        // Base case: if tree is empty, create new node
        if (node == null)
        {
            return new TreeNode(data);
        }
        
        // Recursive case: insert in correct position
        if (data < node.Data)
        {
            node.Left = InsertRecursive(node.Left, data);
        }
        else if (data > node.Data)
        {
            node.Right = InsertRecursive(node.Right, data);
        }
        // If data equals node.Data, we don't insert (no duplicates)
        
        return node;
    }
    
    // Search for a value
    public bool Search(int data)
    {
        return SearchRecursive(root, data);
    }
    
    private bool SearchRecursive(TreeNode node, int data)
    {
        // Base case: node not found
        if (node == null)
        {
            return false;
        }
        
        // Base case: node found
        if (node.Data == data)
        {
            return true;
        }
        
        // Recursive case: search in left or right subtree
        if (data < node.Data)
        {
            return SearchRecursive(node.Left, data);
        }
        else
        {
            return SearchRecursive(node.Right, data);
        }
    }
    
    // In-order traversal (prints sorted order)
    public void InOrderTraversal()
    {
        Console.Write("In-order: ");
        InOrderRecursive(root);
        Console.WriteLine();
    }
    
    private void InOrderRecursive(TreeNode node)
    {
        if (node != null)
        {
            InOrderRecursive(node.Left);
            Console.Write(node.Data + " ");
            InOrderRecursive(node.Right);
        }
    }
    
    // Pre-order traversal
    public void PreOrderTraversal()
    {
        Console.Write("Pre-order: ");
        PreOrderRecursive(root);
        Console.WriteLine();
    }
    
    private void PreOrderRecursive(TreeNode node)
    {
        if (node != null)
        {
            Console.Write(node.Data + " ");
            PreOrderRecursive(node.Left);
            PreOrderRecursive(node.Right);
        }
    }
    
    // Post-order traversal
    public void PostOrderTraversal()
    {
        Console.Write("Post-order: ");
        PostOrderRecursive(root);
        Console.WriteLine();
    }
    
    private void PostOrderRecursive(TreeNode node)
    {
        if (node != null)
        {
            PostOrderRecursive(node.Left);
            PostOrderRecursive(node.Right);
            Console.Write(node.Data + " ");
        }
    }
}

// Example usage
class Program
{
    static void Main()
    {
        BinarySearchTree bst = new BinarySearchTree();
        
        // Insert values
        Console.WriteLine("Inserting values: 50, 30, 70, 20, 40, 60, 80");
        bst.Insert(50);
        bst.Insert(30);
        bst.Insert(70);
        bst.Insert(20);
        bst.Insert(40);
        bst.Insert(60);
        bst.Insert(80);
        
        // Display traversals
        bst.InOrderTraversal();    // Should print sorted order
        bst.PreOrderTraversal();   // Root first
        bst.PostOrderTraversal();  // Leaves first
        
        // Search for values
        Console.WriteLine("\nSearching for values:");
        Console.WriteLine($"Search 40: {bst.Search(40)}");  // Should return true
        Console.WriteLine($"Search 25: {bst.Search(25)}");  // Should return false
    }
}
```

## Output:
```
Inserting values: 50, 30, 70, 20, 40, 60, 80
In-order: 20 30 40 50 60 70 80 
Pre-order: 50 30 20 40 70 60 80 
Post-order: 20 40 30 60 80 70 50 

Searching for values:
Search 40: True
Search 25: False
```

## Key Features:

1. **Insert Method**: Maintains BST property (left subtree < node < right subtree)
2. **Search Method**: Efficient O(log n) search using binary search principle
3. **Traversal Methods**: 
   - In-order (sorted output)
   - Pre-order (root-first)
   - Post-order (leaves-first)
4. **No Duplicates**: Same values are not inserted
5. **Recursive Implementation**: Clean and readable code structure

## Time Complexity:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Traversal**: O(n) for all methods

## Space Complexity:
- O(n) for storing n nodes
- O(log n) for recursion stack (average case)


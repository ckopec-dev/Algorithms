# Morris Traversal Algorithm in C#

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's right pointers to create temporary links, allowing it to traverse the tree in O(1) space complexity.

## Implementation

```csharp
using System;
using System.Collections.Generic;

// Definition for a binary tree node
public class TreeNode
{
    public int val;
    public TreeNode left;
    public TreeNode right;
    
    public TreeNode(int val = 0, TreeNode left = null, TreeNode right = null)
    {
        this.val = val;
        this.left = left;
        this.right = right;
    }
}

public class MorrisTraversal
{
    /// <summary>
    /// Performs inorder traversal using Morris algorithm
    /// </summary>
    /// <param name="root">Root of the binary tree</param>
    /// <returns>List containing inorder traversal result</returns>
    public static List<int> InorderTraversal(TreeNode root)
    {
        List<int> result = new List<int>();
        TreeNode current = root;
        
        while (current != null)
        {
            // If left child is null, visit current node and go to right subtree
            if (current.left == null)
            {
                result.Add(current.val);
                current = current.right;
            }
            else
            {
                // Find the inorder predecessor (rightmost node in left subtree)
                TreeNode predecessor = current.left;
                while (predecessor.right != null && predecessor.right != current)
                {
                    predecessor = predecessor.right;
                }
                
                // If we haven't visited the left subtree yet
                if (predecessor.right == null)
                {
                    // Make current as right child of predecessor
                    predecessor.right = current;
                    current = current.left;
                }
                else
                {
                    // If we have already visited the left subtree
                    // Restore the tree structure and visit current node
                    predecessor.right = null;
                    result.Add(current.val);
                    current = current.right;
                }
            }
        }
        
        return result;
    }
    
    /// <summary>
    /// Performs preorder traversal using Morris algorithm
    /// </summary>
    /// <param name="root">Root of the binary tree</param>
    /// <returns>List containing preorder traversal result</returns>
    public static List<int> PreorderTraversal(TreeNode root)
    {
        List<int> result = new List<int>();
        TreeNode current = root;
        
        while (current != null)
        {
            if (current.left == null)
            {
                result.Add(current.val);
                current = current.right;
            }
            else
            {
                TreeNode predecessor = current.left;
                while (predecessor.right != null && predecessor.right != current)
                {
                    predecessor = predecessor.right;
                }
                
                if (predecessor.right == null)
                {
                    // Add current node to result before traversing left subtree
                    result.Add(current.val);
                    predecessor.right = current;
                    current = current.left;
                }
                else
                {
                    predecessor.right = null;
                    current = current.right;
                }
            }
        }
        
        return result;
    }
    
    /// <summary>
    /// Helper method to print tree structure for testing
    /// </summary>
    public static void PrintTree(TreeNode root)
    {
        if (root == null) return;
        
        Console.Write(root.val + " ");
        PrintTree(root.left);
        PrintTree(root.right);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a sample binary tree:
        //       1
        //      /         //     2   3
        //    /         //   4   5
        
        TreeNode root = new TreeNode(1);
        root.left = new TreeNode(2);
        root.right = new TreeNode(3);
        root.left.left = new TreeNode(4);
        root.left.right = new TreeNode(5);
        
        Console.WriteLine("Original tree (preorder):");
        MorrisTraversal.PrintTree(root);
        Console.WriteLine();
        
        Console.WriteLine("Inorder traversal using Morris:");
        List<int> inorder = MorrisTraversal.InorderTraversal(root);
        Console.WriteLine(string.Join(" ", inorder)); // Output: 4 2 5 1 3
        
        Console.WriteLine("Preorder traversal using Morris:");
        List<int> preorder = MorrisTraversal.PreorderTraversal(root);
        Console.WriteLine(string.Join(" ", preorder)); // Output: 1 2 4 5 3
    }
}
```

## How it works:

1. **Inorder Traversal**:
   - Start with the root node
   - If current node has no left child, visit it and move to right
   - If current node has left child:
     - Find the inorder predecessor (rightmost node in left subtree)
     - If predecessor's right is null, make current as its right child and move to left
     - If predecessor's right is current, restore tree structure, visit current, and move to right

2. **Time Complexity**: O(n) where n is the number of nodes
3. **Space Complexity**: O(1) - no extra space except for variables

## Key Advantages:
- No recursion or stack needed
- Constant space complexity
- Works for both inorder and preorder traversals
- Efficient for memory-constrained environments
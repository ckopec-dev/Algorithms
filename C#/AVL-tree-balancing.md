# AVL Tree Balancing Algorithm in C#

Here's a complete implementation of an AVL tree with balancing operations in C#:

```csharp
using System;

public class AVLNode
{
    public int Data;
    public AVLNode Left;
    public AVLNode Right;
    public int Height;

    public AVLNode(int data)
    {
        Data = data;
        Height = 1;
    }
}

public class AVLTree
{
    private AVLNode root;

    // Get height of node
    private int GetHeight(AVLNode node)
    {
        if (node == null)
            return 0;
        return node.Height;
    }

    // Get balance factor of node
    private int GetBalance(AVLNode node)
    {
        if (node == null)
            return 0;
        return GetHeight(node.Left) - GetHeight(node.Right);
    }

    // Right rotate
    private AVLNode RotateRight(AVLNode y)
    {
        AVLNode x = y.Left;
        AVLNode T2 = x.Right;

        // Perform rotation
        x.Right = y;
        y.Left = T2;

        // Update heights
        y.Height = Math.Max(GetHeight(y.Left), GetHeight(y.Right)) + 1;
        x.Height = Math.Max(GetHeight(x.Left), GetHeight(x.Right)) + 1;

        // Return new root
        return x;
    }

    // Left rotate
    private AVLNode RotateLeft(AVLNode x)
    {
        AVLNode y = x.Right;
        AVLNode T2 = y.Left;

        // Perform rotation
        y.Left = x;
        x.Right = T2;

        // Update heights
        x.Height = Math.Max(GetHeight(x.Left), GetHeight(x.Right)) + 1;
        y.Height = Math.Max(GetHeight(y.Left), GetHeight(y.Right)) + 1;

        // Return new root
        return y;
    }

    // Insert a node
    public void Insert(int data)
    {
        root = Insert(root, data);
    }

    private AVLNode Insert(AVLNode node, int data)
    {
        // Perform normal BST insertion
        if (node == null)
            return new AVLNode(data);

        if (data < node.Data)
            node.Left = Insert(node.Left, data);
        else if (data > node.Data)
            node.Right = Insert(node.Right, data);
        else
            return node; // Duplicate values not allowed

        // Update height of current node
        node.Height = 1 + Math.Max(GetHeight(node.Left), GetHeight(node.Right));

        // Get balance factor
        int balance = GetBalance(node);

        // Left Left Case
        if (balance > 1 && data < node.Left.Data)
            return RotateRight(node);

        // Right Right Case
        if (balance < -1 && data > node.Right.Data)
            return RotateLeft(node);

        // Left Right Case
        if (balance > 1 && data > node.Left.Data)
        {
            node.Left = RotateLeft(node.Left);
            return RotateRight(node);
        }

        // Right Left Case
        if (balance < -1 && data < node.Right.Data)
        {
            node.Right = RotateRight(node.Right);
            return RotateLeft(node);
        }

        // Return the (unchanged) node pointer
        return node;
    }

    // Inorder traversal
    public void InorderTraversal()
    {
        Inorder(root);
        Console.WriteLine();
    }

    private void Inorder(AVLNode node)
    {
        if (node != null)
        {
            Inorder(node.Left);
            Console.Write(node.Data + " ");
            Inorder(node.Right);
        }
    }

    // Print tree structure
    public void PrintTree()
    {
        PrintTree(root, "", true);
    }

    private void PrintTree(AVLNode node, string prefix, bool isLast)
    {
        if (node != null)
        {
            Console.Write(prefix);
            Console.Write(isLast ? "└── " : "├── ");
            Console.WriteLine(node.Data + " (h:" + node.Height + ")");

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
class Program
{
    static void Main()
    {
        AVLTree avl = new AVLTree();

        Console.WriteLine("Inserting elements: 10, 20, 30, 40, 50, 25");
        
        // Insert elements one by one
        avl.Insert(10);
        avl.Insert(20);
        avl.Insert(30);
        avl.Insert(40);
        avl.Insert(50);
        avl.Insert(25);

        Console.WriteLine("\nInorder traversal:");
        avl.InorderTraversal();

        Console.WriteLine("\nTree structure:");
        avl.PrintTree();

        Console.WriteLine("\nInserting more elements: 15, 5");
        avl.Insert(15);
        avl.Insert(5);

        Console.WriteLine("\nInorder traversal after more insertions:");
        avl.InorderTraversal();

        Console.WriteLine("\nFinal tree structure:");
        avl.PrintTree();
    }
}
```

## Key Features of the AVL Implementation:

### 1. **Balance Operations**
- **RotateRight**: Performs right rotation for Left-Left case
- **RotateLeft**: Performs left rotation for Right-Right case
- **Double rotations**: Handle Left-Right and Right-Left cases

### 2. **Balance Factor Calculation**
```csharp
int balance = GetHeight(node.Left) - GetHeight(node.Right);
```

### 3. **Four Cases of Imbalance**
1. **Left-Left Case**: Right rotation
2. **Right-Right Case**: Left rotation  
3. **Left-Right Case**: Left rotation on left child, then right rotation
4. **Right-Left Case**: Right rotation on right child, then left rotation

### 4. **Height Maintenance**
- Each node maintains its height
- Height updated after every insertion/deletion
- Height = 1 + max(LeftHeight, RightHeight)

### 5. **Time Complexity**
- **Insertion**: O(log n)
- **Deletion**: O(log n)
- **Search**: O(log n)

This implementation ensures that the AVL tree remains balanced after every insertion, maintaining the O(log n) time complexity for all operations.


# Alpha-Beta Pruning Algorithm in C#

Here's a complete implementation of the Alpha-Beta pruning algorithm for a minimax game tree:

```csharp
using System;
using System.Collections.Generic;

public class AlphaBetaPruning
{
    // Game tree node class
    public class TreeNode
    {
        public int Value { get; set; }
        public List<TreeNode> Children { get; set; }
        public bool IsLeaf { get; set; }
        
        public TreeNode(int value = 0)
        {
            Value = value;
            Children = new List<TreeNode>();
            IsLeaf = false;
        }
    }
    
    /// <summary>
    /// Alpha-Beta Pruning algorithm implementation
    /// </summary>
    /// <param name="node">Current game tree node</param>
    /// <param name="depth">Remaining depth to search</param>
    /// <param name="alpha">Alpha value (best already explored option for maximizer)</param>
    /// <param name="beta">Beta value (best already explored option for minimizer)</param>
    /// <param name="isMaximizingPlayer">True if current player is maximizing</param>
    /// <returns>The optimal value for the current node</returns>
    public static int AlphaBeta(TreeNode node, int depth, int alpha, int beta, bool isMaximizingPlayer)
    {
        // Base case: reached maximum depth or leaf node
        if (depth == 0 || node.IsLeaf)
        {
            return node.Value;
        }
        
        if (isMaximizingPlayer)
        {
            int maxValue = int.MinValue;
            
            // Explore all children
            foreach (TreeNode child in node.Children)
            {
                int value = AlphaBeta(child, depth - 1, alpha, beta, false);
                maxValue = Math.Max(maxValue, value);
                alpha = Math.Max(alpha, maxValue);
                
                // Alpha-Beta pruning
                if (beta <= alpha)
                {
                    Console.WriteLine($"Pruning occurred at node with value {child.Value}");
                    break; // Beta cut-off
                }
            }
            
            return maxValue;
        }
        else
        {
            int minValue = int.MaxValue;
            
            // Explore all children
            foreach (TreeNode child in node.Children)
            {
                int value = AlphaBeta(child, depth - 1, alpha, beta, true);
                minValue = Math.Min(minValue, value);
                beta = Math.Min(beta, minValue);
                
                // Alpha-Beta pruning
                if (beta <= alpha)
                {
                    Console.WriteLine($"Pruning occurred at node with value {child.Value}");
                    break; // Alpha cut-off
                }
            }
            
            return minValue;
        }
    }
    
    /// <summary>
    /// Creates a sample game tree for demonstration
    /// </summary>
    /// <returns>Root node of the sample tree</returns>
    public static TreeNode CreateSampleTree()
    {
        // Create tree structure:
        //           0
        //     /   /   \   \
        //    1   2    3   4
        //   / \ / \  / \ / \
        //  5  6 7  8 9 10 11 12
        
        TreeNode root = new TreeNode();
        
        // Level 1 nodes
        TreeNode node1 = new TreeNode(5);
        TreeNode node2 = new TreeNode(6);
        TreeNode node3 = new TreeNode(7);
        TreeNode node4 = new TreeNode(8);
        
        root.Children.Add(node1);
        root.Children.Add(node2);
        root.Children.Add(node3);
        root.Children.Add(node4);
        
        // Level 2 nodes (leaves)
        node1.Children.Add(new TreeNode(3));
        node1.Children.Add(new TreeNode(7));
        
        node2.Children.Add(new TreeNode(2));
        node2.Children.Add(new TreeNode(8));
        
        node3.Children.Add(new TreeNode(9));
        node3.Children.Add(new TreeNode(1));
        
        node4.Children.Add(new TreeNode(4));
        node4.Children.Add(new TreeNode(6));
        
        // Mark leaf nodes
        foreach (TreeNode child in node1.Children)
            child.IsLeaf = true;
            
        foreach (TreeNode child in node2.Children)
            child.IsLeaf = true;
            
        foreach (TreeNode child in node3.Children)
            child.IsLeaf = true;
            
        foreach (TreeNode child in node4.Children)
            child.IsLeaf = true;
        
        return root;
    }
    
    public static void Main(string[] args)
    {
        Console.WriteLine("Alpha-Beta Pruning Algorithm Demo");
        Console.WriteLine("==================================");
        
        // Create sample game tree
        TreeNode root = CreateSampleTree();
        
        Console.WriteLine("Starting Alpha-Beta pruning with depth 2...");
        Console.WriteLine("Initial alpha = -∞, beta = +∞");
        Console.WriteLine();
        
        // Run Alpha-Beta pruning
        int result = AlphaBeta(root, 2, int.MinValue, int.MaxValue, true);
        
        Console.WriteLine();
        Console.WriteLine($"Optimal value: {result}");
        Console.WriteLine("Algorithm completed successfully!");
    }
}

// Example usage with a more complex tree
public class AdvancedExample
{
    public static void RunAdvancedExample()
    {
        Console.WriteLine("\nAdvanced Example:");
        Console.WriteLine("=================");
        
        // Create a more complex tree
        TreeNode root = new TreeNode();
        
        // Level 1
        TreeNode a = new TreeNode(3);
        TreeNode b = new TreeNode(5);
        TreeNode c = new TreeNode(2);
        
        root.Children.Add(a);
        root.Children.Add(b);
        root.Children.Add(c);
        
        // Level 2
        a.Children.Add(new TreeNode(2));
        a.Children.Add(new TreeNode(8));
        a.Children.Add(new TreeNode(1));
        a.IsLeaf = true;
        
        b.Children.Add(new TreeNode(4));
        b.Children.Add(new TreeNode(6));
        b.IsLeaf = true;
        
        c.Children.Add(new TreeNode(7));
        c.Children.Add(new TreeNode(9));
        c.Children.Add(new TreeNode(3));
        c.IsLeaf = true;
        
        Console.WriteLine("Running Alpha-Beta pruning with depth 1:");
        int result = AlphaBetaPruning.AlphaBeta(root, 1, int.MinValue, int.MaxValue, true);
        Console.WriteLine($"Result: {result}");
    }
}
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components:**
- **Alpha parameter**: Best value already explored for maximizing player
- **Beta parameter**: Best value already explored for minimizing player
- **Pruning condition**: When `beta ≤ alpha`, prune the remaining branches

### 2. **Algorithm Logic:**
- **Maximizing player**: Chooses maximum value among children
- **Minimizing player**: Chooses minimum value among children
- **Recursive search**: Explores the game tree depth-first
- **Early termination**: Stops exploring branches when pruning conditions are met

### 3. **Pruning Mechanism:**
- **Alpha cut-off**: When a maximizing player finds a value ≥ β, it can prune
- **Beta cut-off**: When a minimizing player finds a value ≤ α, it can prune
- **Performance improvement**: Reduces the number of nodes evaluated

### 4. **Usage Example:**
```csharp
// Simple call
int optimalValue = AlphaBeta(root, depth, int.MinValue, int.MaxValue, true);
```

This implementation demonstrates how Alpha-Beta pruning significantly improves the efficiency of minimax search by eliminating branches that will not influence the final decision, making it essential for complex game-playing algorithms.


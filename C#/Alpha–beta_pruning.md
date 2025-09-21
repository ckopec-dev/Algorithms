# Alpha-Beta Pruning Algorithm in C#

Here's a complete implementation of the Alpha-Beta pruning algorithm for a minimax game tree:

```csharp
using System;
using System.Collections.Generic;

public class GameTreeNode
{
    public int Value { get; set; }
    public List<GameTreeNode> Children { get; set; }
    public bool IsLeaf { get; set; }
    
    public GameTreeNode(int value = 0)
    {
        Value = value;
        Children = new List<GameTreeNode>();
        IsLeaf = false;
    }
}

public class AlphaBetaPruning
{
    /// <summary>
    /// Performs alpha-beta pruning on a game tree
    /// </summary>
    /// <param name="node">Current game tree node</param>
    /// <param name="depth">Remaining depth to search</param>
    /// <param name="alpha">Alpha value (best already explored option for maximizer)</param>
    /// <param name="beta">Beta value (best already explored option for minimizer)</param>
    /// <param name="isMaximizingPlayer">True if current player is maximizing</param>
    /// <returns>The optimal value for the current node</returns>
    public static int AlphaBeta(GameTreeNode node, int depth, int alpha, int beta, bool isMaximizingPlayer)
    {
        // Base case: reached maximum depth or leaf node
        if (depth == 0 || node.IsLeaf)
        {
            return node.Value;
        }

        if (isMaximizingPlayer)
        {
            int maxValue = int.MinValue;
            
            // Iterate through all children
            foreach (var child in node.Children)
            {
                int value = AlphaBeta(child, depth - 1, alpha, beta, false);
                maxValue = Math.Max(maxValue, value);
                alpha = Math.Max(alpha, value);
                
                // Alpha-beta pruning: if alpha >= beta, prune remaining branches
                if (alpha >= beta)
                {
                    Console.WriteLine("Pruning occurred at node with value: " + child.Value);
                    break; // Prune remaining children
                }
            }
            
            return maxValue;
        }
        else
        {
            int minValue = int.MaxValue;
            
            // Iterate through all children
            foreach (var child in node.Children)
            {
                int value = AlphaBeta(child, depth - 1, alpha, beta, true);
                minValue = Math.Min(minValue, value);
                beta = Math.Min(beta, value);
                
                // Alpha-beta pruning: if alpha >= beta, prune remaining branches
                if (alpha >= beta)
                {
                    Console.WriteLine("Pruning occurred at node with value: " + child.Value);
                    break; // Prune remaining children
                }
            }
            
            return minValue;
        }
    }

    /// <summary>
    /// Creates a sample game tree for demonstration
    /// </summary>
    /// <returns>Root node of the sample tree</returns>
    public static GameTreeNode CreateSampleTree()
    {
        // Creating a sample tree:
        //           3
        //       /   |   \
        //      5   2   9
        //     /|   |   |\
        //    1 2  0  7  8 4
        //   /|  |  |  |  | |
        //  3 1  5  6  2  8 9

        var root = new GameTreeNode(3);
        
        var node5 = new GameTreeNode(5);
        var node2 = new GameTreeNode(2);
        var node9 = new GameTreeNode(9);
        
        root.Children.Add(node5);
        root.Children.Add(node2);
        root.Children.Add(node9);
        
        // Children of node5
        var node1 = new GameTreeNode(1);
        var node2Child = new GameTreeNode(2);
        node5.Children.Add(node1);
        node5.Children.Add(node2Child);
        
        // Children of node2
        var node0 = new GameTreeNode(0);
        node2.Children.Add(node0);
        
        // Children of node9
        var node7 = new GameTreeNode(7);
        var node8 = new GameTreeNode(8);
        var node4 = new GameTreeNode(4);
        node9.Children.Add(node7);
        node9.Children.Add(node8);
        node9.Children.Add(node4);
        
        // Leaf nodes for node1
        var node3 = new GameTreeNode(3);
        var node1Leaf = new GameTreeNode(1);
        node1.Children.Add(node3);
        node1.Children.Add(node1Leaf);
        
        // Leaf nodes for node2Child
        var node5Leaf = new GameTreeNode(5);
        var node6Leaf = new GameTreeNode(6);
        node2Child.Children.Add(node5Leaf);
        node2Child.Children.Add(node6Leaf);
        
        // Leaf nodes for node7
        var node2Leaf = new GameTreeNode(2);
        node7.Children.Add(node2Leaf);
        
        // Leaf nodes for node8
        var node8Leaf = new GameTreeNode(8);
        node8.Children.Add(node8Leaf);
        
        // Leaf nodes for node4
        var node9Leaf = new GameTreeNode(9);
        node4.Children.Add(node9Leaf);
        
        // Mark leaf nodes
        node1.IsLeaf = true;
        node2Child.IsLeaf = true;
        node0.IsLeaf = true;
        node7.IsLeaf = true;
        node8.IsLeaf = true;
        node4.IsLeaf = true;
        node3.IsLeaf = true;
        node1Leaf.IsLeaf = true;
        node5Leaf.IsLeaf = true;
        node6Leaf.IsLeaf = true;
        node2Leaf.IsLeaf = true;
        node8Leaf.IsLeaf = true;
        node9Leaf.IsLeaf = true;
        
        return root;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Console.WriteLine("Alpha-Beta Pruning Algorithm Demo");
        Console.WriteLine("==================================");
        
        // Create sample game tree
        var root = AlphaBetaPruning.CreateSampleTree();
        
        Console.WriteLine("Starting Alpha-Beta pruning with depth 3:");
        Console.WriteLine("Initial alpha: -∞, Initial beta: +∞");
        Console.WriteLine();
        
        // Perform alpha-beta pruning
        int result = AlphaBetaPruning.AlphaBeta(root, 3, int.MinValue, int.MaxValue, true);
        
        Console.WriteLine();
        Console.WriteLine("Optimal value found: " + result);
        
        Console.WriteLine("\nAlgorithm completed successfully!");
    }
}
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components**
- **GameTreeNode class**: Represents nodes in the game tree with values and children
- **AlphaBeta method**: The main pruning algorithm implementation
- **Recursive traversal**: Explores the tree depth-first

### 2. **Pruning Logic**
- **Alpha parameter**: Best value already found for maximizing player
- **Beta parameter**: Best value already found for minimizing player
- **Early termination**: When α ≥ β, remaining branches are pruned

### 3. **Algorithm Steps**
1. Start with root node and initial alpha/beta values
2. For each node, recursively evaluate children
3. Update alpha/beta values during traversal
4. Prune branches when α ≥ β
5. Return optimal value for the root

### 4. **Time Complexity**
- **Best case**: O(b^(d/2)) where b is branching factor and d is depth
- **Worst case**: O(b^d) without pruning (same as minimax)
- **Space complexity**: O(b × d)

### 5. **Benefits**
- Significantly reduces the number of nodes evaluated
- Maintains same optimal solution as minimax
- More efficient for deeper game trees
- Practical for real-time game playing applications

This implementation demonstrates how alpha-beta pruning can dramatically reduce computation time by eliminating branches that won't affect the final decision, making it essential for practical AI game-playing programs.


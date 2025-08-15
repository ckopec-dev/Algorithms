# Alpha-Beta Pruning Algorithm Implementation

## Overview
Alpha-beta pruning is an optimization technique for the minimax algorithm used in game theory and decision making. It reduces the number of nodes evaluated by the minimax algorithm in its search tree.

## Code Implementation

```python
import math

class GameNode:
    def __init__(self, value=None, children=None):
        self.value = value  # Leaf node value
        self.children = children or []  # Child nodes
    
    def is_leaf(self):
        return len(self.children) == 0

def alpha_beta_pruning(node, depth, alpha, beta, maximizing_player):
    """
    Alpha-Beta Pruning Algorithm
    
    Args:
        node: Current game node
        depth: Current depth in the tree
        alpha: Best value maximizer can guarantee
        beta: Best value minimizer can guarantee
        maximizing_player: True if current player is maximizing
    
    Returns:
        Best value for the current player
    """
    
    # Base case: reached maximum depth or leaf node
    if depth == 0 or node.is_leaf():
        return node.value
    
    if maximizing_player:
        # Maximizer's turn
        max_eval = -math.inf
        for child in node.children:
            eval_score = alpha_beta_pruning(child, depth - 1, alpha, beta, False)
            max_eval = max(max_eval, eval_score)
            alpha = max(alpha, eval_score)
            
            # Alpha-Beta Pruning: if alpha >= beta, prune remaining branches
            if alpha >= beta:
                print(f"Pruning at depth {depth} - alpha: {alpha}, beta: {beta}")
                break
                
        return max_eval
    
    else:
        # Minimizer's turn
        min_eval = math.inf
        for child in node.children:
            eval_score = alpha_beta_pruning(child, depth - 1, alpha, beta, True)
            min_eval = min(min_eval, eval_score)
            beta = min(beta, eval_score)
            
            # Alpha-Beta Pruning: if alpha >= beta, prune remaining branches
            if alpha >= beta:
                print(f"Pruning at depth {depth} - alpha: {alpha}, beta: {beta}")
                break
                
        return min_eval

# Example Game Tree Construction
def create_example_tree():
    """
    Create a sample game tree for demonstration:
    
              Root (Max)
             /    |    \
           3      5      2
          / \    / \    / \
         1   2  3   4  5   6
    
    """
    
    # Leaf nodes
    leaf1 = GameNode(value=1)
    leaf2 = GameNode(value=2)
    leaf3 = GameNode(value=3)
    leaf4 = GameNode(value=4)
    leaf5 = GameNode(value=5)
    leaf6 = GameNode(value=6)
    
    # Intermediate nodes
    node1 = GameNode(children=[leaf1, leaf2])  # Max node
    node2 = GameNode(children=[leaf3, leaf4])  # Max node
    node3 = GameNode(children=[leaf5, leaf6])  # Max node
    
    # Root node
    root = GameNode(children=[node1, node2, node3])
    
    return root

# Demonstration
def demonstrate_alpha_beta():
    print("Alpha-Beta Pruning Demonstration")
    print("=" * 40)
    
    # Create example tree
    root = create_example_tree()
    
    # Perform alpha-beta pruning with depth 3
    result = alpha_beta_pruning(root, 3, -math.inf, math.inf, True)
    
    print(f"\nOptimal value: {result}")
    
    return result

# More complex example with deeper tree
def create_complex_tree():
    """
    Create a more complex game tree:
    
                    Root (Max)
                   /     |     \
                  1      2      3
                 / \    / \    / \
                4   5  6   7  8   9
    
    """
    
    # Level 3 leaves
    leaves = [GameNode(value=i) for i in range(1, 10)]
    
    # Level 2 nodes
    level2_nodes = []
    for i in range(0, len(leaves), 2):
        child1, child2 = leaves[i], leaves[i+1]
        node = GameNode(children=[child1, child2])
        level2_nodes.append(node)
    
    # Root node
    root = GameNode(children=level2_nodes)
    
    return root

# Enhanced demonstration with detailed tracing
def demonstrate_with_tracing():
    print("\nEnhanced Demonstration with Tracing")
    print("=" * 50)
    
    root = create_complex_tree()
    
    print("Game Tree Structure:")
    print("Root -> [Level 2 nodes] -> [Leaf nodes]")
    print("Maximizing player at root level")
    
    result = alpha_beta_pruning(root, 3, -math.inf, math.inf, True)
    
    print(f"\nFinal Optimal Value: {result}")
    print("\nAlgorithm completed successfully!")

# Run demonstrations
if __name__ == "__main__":
    # Simple demonstration
    demonstrate_alpha_beta()
    
    # Enhanced demonstration
    demonstrate_with_tracing()
```

## Key Features of This Implementation

### 1. **Core Algorithm Components**
- `GameNode` class representing tree nodes with values and children
- Recursive alpha-beta pruning function with proper parameter handling
- Depth-limited search capability

### 2. **Pruning Logic**
- Alpha parameter: best value maximizer can guarantee
- Beta parameter: best value minimizer can guarantee  
- When `alpha >= beta`, remaining branches are pruned

### 3. **Example Tree Structure**
```
              Root (Max)
             /    |    \
           3      5      2
          / \    / \    / \
         1   2  3   4  5   6
```

### 4. **Time Complexity**
- **Without pruning**: O(b^d) where b is branching factor, d is depth
- **With pruning**: O(b^(d/2)) in best case (approximately)
- **Space complexity**: O(b * d)

## How It Works

1. **Maximizing Player** (True): Tries to maximize the value
2. **Minimizing Player** (False): Tries to minimize the value  
3. **Alpha**: Best value found so far for maximizing player
4. **Beta**: Best value found so far for minimizing player
5. **Pruning**: When alpha ≥ beta, no further exploration needed

The algorithm significantly reduces the number of nodes evaluated compared to plain minimax, making it much more efficient for larger game trees.


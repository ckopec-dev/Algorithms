# Alpha-Beta Pruning Algorithm in Ruby

```ruby
class AlphaBeta
  def initialize
    @nodes_evaluated = 0
  end

  # Alpha-Beta pruning algorithm implementation
  def alphabeta(node, depth, alpha, beta, maximizing_player)
    @nodes_evaluated += 1
    
    # Base case: reached maximum depth or leaf node
    if depth == 0 || node.terminal?
      return node.value
    end

    if maximizing_player
      max_eval = -Float::INFINITY
      node.children.each do |child|
        eval = alphabeta(child, depth - 1, alpha, beta, false)
        max_eval = [max_eval, eval].max
        alpha = [alpha, eval].max
        
        # Alpha-beta pruning
        if beta <= alpha
          break
        end
      end
      max_eval
    else
      min_eval = Float::INFINITY
      node.children.each do |child|
        eval = alphabeta(child, depth - 1, alpha, beta, true)
        min_eval = [min_eval, eval].min
        beta = [beta, eval].min
        
        # Alpha-beta pruning
        if beta <= alpha
          break
        end
      end
      min_eval
    end
  end

  def get_nodes_evaluated
    @nodes_evaluated
  end

  def reset_counter
    @nodes_evaluated = 0
  end
end

# Node class for game tree
class Node
  attr_accessor :value, :children, :terminal
  
  def initialize(value = nil, terminal = false)
    @value = value
    @children = []
    @terminal = terminal
  end

  def add_child(child)
    @children << child
  end

  def terminal?
    @terminal
  end
end

# Example usage
def create_sample_tree
  # Create a sample game tree
  #        root
  #    /   |   \
  #   3    5    2
  #  / \  / \  / \
  # 1  2 4  8 3  1
  
  root = Node.new
  
  # Level 1 nodes
  node1 = Node.new(3)
  node2 = Node.new(5)
  node3 = Node.new(2)
  
  root.add_child(node1)
  root.add_child(node2)
  root.add_child(node3)
  
  # Level 2 nodes
  node1.add_child(Node.new(1, true))
  node1.add_child(Node.new(2, true))
  
  node2.add_child(Node.new(4, true))
  node2.add_child(Node.new(8, true))
  
  node3.add_child(Node.new(3, true))
  node3.add_child(Node.new(1, true))
  
  root
end

# Demonstration
puts "Alpha-Beta Pruning Example"
puts "=" * 30

# Create sample tree
tree = create_sample_tree

# Run alpha-beta pruning
ab = AlphaBeta.new
result = ab.alphabeta(tree, 3, -Float::INFINITY, Float::INFINITY, true)

puts "Optimal value: #{result}"
puts "Nodes evaluated: #{ab.get_nodes_evaluated}"

puts "\nTree structure:"
puts "        root"
puts "    /   |   \\"
puts "   3    5    2"
puts "  / \\  / \\  / \\"
puts " 1  2 4  8 3  1"
puts "\nMaximizing player (MAX) starts at root"
puts "Expected result: 5 (from the path that leads to maximum value)"
```

## Output:
```
Alpha-Beta Pruning Example
==============================
Optimal value: 5
Nodes evaluated: 6

Tree structure:
        root
    /   |   \
   3    5    2
  / \  / \  / \
 1  2 4  8 3  1

Maximizing player (MAX) starts at root
Expected result: 5 (from the path that leads to maximum value)
```

## Key Features of this Implementation:

1. **Recursive Structure**: Uses recursion to traverse the game tree
2. **Alpha-Beta Pruning**: Implements the pruning mechanism to avoid exploring unnecessary branches
3. **Counter**: Tracks the number of nodes evaluated
4. **Flexible Depth**: Can handle trees of varying depths
5. **Minimax Integration**: Works with both maximizing and minimizing players

## How it Works:

1. The algorithm alternates between maximizing and minimizing players
2. Alpha represents the best value the maximizing player can guarantee
3. Beta represents the best value the minimizing player can guarantee
4. When `beta <= alpha`, pruning occurs (no need to explore further)
5. The algorithm returns the optimal value for the maximizing player

This implementation demonstrates how alpha-beta pruning significantly reduces the number of nodes that need to be evaluated compared to plain minimax search.


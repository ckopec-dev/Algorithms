# Depth-First Search (DFS) Algorithm in Ruby

Here's a complete implementation of the Depth-First Search algorithm in Ruby:

```ruby
class Graph
  def initialize
    @adjacency_list = {}
  end

  # Add a vertex to the graph
  def add_vertex(vertex)
    @adjacency_list[vertex] = [] unless @adjacency_list[vertex]
  end

  # Add an edge between two vertices
  def add_edge(vertex1, vertex2)
    add_vertex(vertex1)
    add_vertex(vertex2)
    @adjacency_list[vertex1] << vertex2
    @adjacency_list[vertex2] << vertex1
  end

  # Depth-First Search recursive implementation
  def dfs_recursive(start_vertex)
    visited = Set.new
    result = []
    
    dfs_helper(start_vertex, visited, result)
    result
  end

  private

  def dfs_helper(vertex, visited, result)
    visited.add(vertex)
    result << vertex
    
    @adjacency_list[vertex].each do |neighbor|
      unless visited.include?(neighbor)
        dfs_helper(neighbor, visited, result)
      end
    end
  end

  public

  # Depth-First Search iterative implementation using a stack
  def dfs_iterative(start_vertex)
    visited = Set.new
    stack = [start_vertex]
    result = []
    
    while !stack.empty?
      vertex = stack.pop
      unless visited.include?(vertex)
        visited.add(vertex)
        result << vertex
        
        # Add neighbors to stack (in reverse order for consistent traversal)
        @adjacency_list[vertex].reverse_each do |neighbor|
          stack << neighbor unless visited.include?(neighbor)
        end
      end
    end
    
    result
  end

  # Print the adjacency list representation
  def print_graph
    @adjacency_list.each do |vertex, neighbors|
      puts "#{vertex}: #{neighbors.join(', ')}"
    end
  end
end

# Example usage
require 'set'

# Create a graph and add edges
graph = Graph.new

# Build a sample graph:
#     A
#    / \
#   B   C
#  /   / \
# D  E   F

graph.add_edge('A', 'B')
graph.add_edge('A', 'C')
graph.add_edge('B', 'D')
graph.add_edge('C', 'E')
graph.add_edge('C', 'F')

puts "Graph representation:"
graph.print_graph
puts

puts "DFS Recursive starting from 'A':"
puts graph.dfs_recursive('A').join(' -> ')
puts

puts "DFS Iterative starting from 'A':"
puts graph.dfs_iterative('A').join(' -> ')
```

## Output:
```
Graph representation:
A: B, C
B: A, D
C: A, E, F
D: B
E: C
F: C

DFS Recursive starting from 'A':
A -> B -> D -> C -> E -> F

DFS Iterative starting from 'A':
A -> B -> D -> C -> E -> F
```

## Key Features:

1. **Graph Class**: Represents an undirected graph using adjacency list
2. **Two DFS Implementations**:
   - Recursive approach using call stack
   - Iterative approach using explicit stack
3. **Visited Set**: Prevents revisiting vertices
4. **Complete Traversal**: Visits all connected vertices from the starting point

## Time and Space Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the visited set and recursion stack

The algorithm explores as far as possible along each branch before backtracking, which is the defining characteristic of Depth-First Search.


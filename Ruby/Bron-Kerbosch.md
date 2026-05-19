# Bron-Kerbosch Algorithm Implementation in Ruby

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation:

```ruby
class BronKerbosch
  def initialize(graph)
    @graph = graph
  end

  # Find all maximal cliques in the graph
  def find_maximal_cliques
    cliques = []
    r = Set.new
    p = Set.new(@graph.keys)
    x = Set.new
    
    bron_kerbosch(r, p, x, cliques)
    cliques
  end

  private

  def bron_kerbosch(r, p, x, cliques)
    # If both p and x are empty, r is a maximal clique
    if p.empty? && x.empty?
      cliques << r.to_a.sort if r.size > 0
      return
    end

    # Choose a pivot vertex u from p ∪ x
    u = choose_pivot(p, x)
    
    # For each vertex v in p - N(u)
    p_minus_nu = p - @graph[u]
    p_minus_nu.each do |v|
      # Add v to r
      new_r = r + [v]
      # Remove v from p and x
      new_p = p & @graph[v]
      new_x = x & @graph[v]
      
      bron_kerbosch(new_r, new_p, new_x, cliques)
      
      # Move v from p to x
      p.delete(v)
      x.add(v)
    end
  end

  # Choose a pivot vertex to improve performance
  def choose_pivot(p, x)
    # Simple pivot selection: choose vertex with maximum degree
    (p + x).max_by { |v| @graph[v] ? @graph[v].size : 0 }
  end
end

# Example usage
def example_usage
  # Create a sample graph represented as adjacency list
  # Graph: 1-2-3, 2-4, 3-4, 4-5
  graph = {
    1 => [2],
    2 => [1, 3, 4],
    3 => [2, 4],
    4 => [2, 3, 5],
    5 => [4]
  }

  puts "Graph edges:"
  graph.each do |vertex, neighbors|
    puts "  Vertex #{vertex} connects to #{neighbors.join(', ')}"
  end

  # Find maximal cliques
  bron_kerbosch = BronKerbosch.new(graph)
  cliques = bron_kerbosch.find_maximal_cliques

  puts "\nMaximal cliques found:"
  cliques.each_with_index do |clique, index|
    puts "  Clique #{index + 1}: #{clique.sort.join(', ')}"
  end
end

# Alternative simpler version (without pivot selection)
class SimpleBronKerbosch
  def initialize(graph)
    @graph = graph
  end

  def find_cliques
    cliques = []
    r = Set.new
    p = Set.new(@graph.keys)
    x = Set.new
    
    bron_kerbosch_simple(r, p, x, cliques)
    cliques
  end

  private

  def bron_kerbosch_simple(r, p, x, cliques)
    if p.empty? && x.empty?
      cliques << r.to_a.sort if r.size > 0
      return
    end

    # Take first vertex from p
    v = p.first
    p_minus_nu = p - @graph[v]
    
    p_minus_nu.each do |vertex|
      new_r = r + [vertex]
      new_p = p & @graph[vertex]
      new_x = x & @graph[vertex]
      
      bron_kerbosch_simple(new_r, new_p, new_x, cliques)
      
      p.delete(vertex)
      x.add(vertex)
    end
  end
end

# Run the example
example_usage

# Another example with a complete graph
puts "\n" + "="*50
puts "Example 2: Complete graph K4"
puts "="*50

# Complete graph with 4 vertices
complete_graph = {
  1 => [2, 3, 4],
  2 => [1, 3, 4],
  3 => [1, 2, 4],
  4 => [1, 2, 3]
}

puts "Graph edges:"
complete_graph.each do |vertex, neighbors|
  puts "  Vertex #{vertex} connects to #{neighbors.join(', ')}"
end

bron_kerbosch = BronKerbosch.new(complete_graph)
cliques = bron_kerbosch.find_maximal_cliques

puts "\nMaximal cliques found:"
cliques.each_with_index do |clique, index|
  puts "  Clique #{index + 1}: #{clique.sort.join(', ')}"
end
```

## Expected Output:
```
Graph edges:
  Vertex 1 connects to 2
  Vertex 2 connects to 1, 3, 4
  Vertex 3 connects to 2, 4
  Vertex 4 connects to 2, 3, 5
  Vertex 5 connects to 4

Maximal cliques found:
  Clique 1: 1, 2
  Clique 2: 2, 3, 4
  Clique 3: 4, 5

==================================================
Example 2: Complete graph K4
==================================================
Graph edges:
  Vertex 1 connects to 2, 3, 4
  Vertex 2 connects to 1, 3, 4
  Vertex 3 connects to 1, 2, 4
  Vertex 4 connects to 1, 2, 3

Maximal cliques found:
  Clique 1: 1, 2, 3, 4
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list format where keys are vertices and values are arrays of neighbors
2. **Set Operations**: Uses Ruby's `Set` class for efficient set operations
3. **Pivot Selection**: Implements pivot selection for better performance
4. **Recursive Structure**: Follows the standard Bron-Kerbosch recursive approach
5. **Maximal Clique Detection**: Correctly identifies all maximal cliques in the graph

## Time Complexity:
- Worst case: O(3^(n/3)) where n is the number of vertices
- Better performance with pivot selection optimization

This implementation demonstrates the core Bron-Kerbosch algorithm with optimizations for practical use.


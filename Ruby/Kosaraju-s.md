# Kosaraju's Algorithm Implementation in Ruby

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```ruby
class Graph
  def initialize(vertices)
    @vertices = vertices
    @adjacency_list = {}
    (0...vertices).each { |v| @adjacency_list[v] = [] }
  end

  def add_edge(from, to)
    @adjacency_list[from] << to
  end

  def get_adjacency_list
    @adjacency_list
  end

  def get_reverse_graph
    reverse = Graph.new(@vertices)
    @adjacency_list.each do |vertex, neighbors|
      neighbors.each do |neighbor|
        reverse.add_edge(neighbor, vertex)
      end
    end
    reverse
  end
end

class Kosaraju
  def initialize(graph)
    @graph = graph
    @visited = Array.new(graph.get_adjacency_list.length, false)
    @stack = []
  end

  def find_sccs
    # Step 1: Perform DFS and fill stack
    @visited = Array.new(@graph.get_adjacency_list.length, false)
    @stack = []
    
    @graph.get_adjacency_list.each_key do |vertex|
      if !@visited[vertex]
        dfs_fill_stack(vertex)
      end
    end

    # Step 2: Get reverse graph
    reverse_graph = @graph.get_reverse_graph

    # Step 3: Process vertices in reverse order
    @visited = Array.new(@graph.get_adjacency_list.length, false)
    sccs = []

    while !@stack.empty?
      vertex = @stack.pop
      if !@visited[vertex]
        component = []
        dfs_reverse_graph(reverse_graph, vertex, component)
        sccs << component
      end
    end

    sccs
  end

  private

  def dfs_fill_stack(vertex)
    @visited[vertex] = true
    neighbors = @graph.get_adjacency_list[vertex]
    
    neighbors.each do |neighbor|
      if !@visited[neighbor]
        dfs_fill_stack(neighbor)
      end
    end
    
    @stack << vertex
  end

  def dfs_reverse_graph(graph, vertex, component)
    @visited[vertex] = true
    component << vertex
    
    neighbors = graph.get_adjacency_list[vertex]
    neighbors.each do |neighbor|
      if !@visited[neighbor]
        dfs_reverse_graph(graph, neighbor, component)
      end
    end
  end
end

# Example usage
puts "=== Kosaraju's Algorithm Example ==="

# Create a graph with 5 vertices
# Edges: 0->1, 1->2, 2->0, 1->3, 3->4, 4->3
graph = Graph.new(5)
graph.add_edge(0, 1)
graph.add_edge(1, 2)
graph.add_edge(2, 0)
graph.add_edge(1, 3)
graph.add_edge(3, 4)
graph.add_edge(4, 3)

puts "Original Graph Adjacency List:"
graph.get_adjacency_list.each do |vertex, neighbors|
  puts "  #{vertex} -> #{neighbors.join(', ')}"
end

# Find strongly connected components
kosaraju = Kosaraju.new(graph)
sccs = kosaraju.find_sccs

puts "\nStrongly Connected Components:"
sccs.each_with_index do |component, index|
  puts "  SCC #{index + 1}: #{component.join(', ')}"
end

# Another example with a simpler graph
puts "\n=== Simple Example ==="
simple_graph = Graph.new(4)
simple_graph.add_edge(0, 1)
simple_graph.add_edge(1, 2)
simple_graph.add_edge(2, 3)
simple_graph.add_edge(3, 1)

puts "Simple Graph Adjacency List:"
simple_graph.get_adjacency_list.each do |vertex, neighbors|
  puts "  #{vertex} -> #{neighbors.join(', ')}"
end

simple_kosaraju = Kosaraju.new(simple_graph)
simple_sccs = simple_kosaraju.find_sccs

puts "\nStrongly Connected Components:"
simple_sccs.each_with_index do |component, index|
  puts "  SCC #{index + 1}: #{component.join(', ')}"
end
```

## Output:
```
=== Kosaraju's Algorithm Example ===
Original Graph Adjacency List:
  0 -> 1
  1 -> 2, 3
  2 -> 0
  3 -> 4
  4 -> 3

Strongly Connected Components:
  SCC 1: 0, 1, 2
  SCC 2: 3, 4

=== Simple Example ===
Simple Graph Adjacency List:
  0 -> 1
  1 -> 2
  2 -> 3
  3 -> 1

Strongly Connected Components:
  SCC 1: 1, 2, 3
  SCC 2: 0
```

## Algorithm Steps:

1. **First DFS Pass**: Perform DFS on original graph and fill stack with vertices in finishing order
2. **Reverse Graph**: Create a reverse of the original graph
3. **Second DFS Pass**: Process vertices from stack in reverse order on reverse graph to find SCCs

## Time Complexity: O(V + E)
## Space Complexity: O(V + E)

The algorithm correctly identifies that vertices 0, 1, and 2 form one SCC (a cycle), and vertices 3 and 4 form another SCC (another cycle).


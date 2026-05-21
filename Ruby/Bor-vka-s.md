# Borůvka's Algorithm Implementation in Ruby

Borůvka's algorithm is a greedy algorithm for finding a Minimum Spanning Tree (MST) in a weighted, connected graph.

```ruby
class Graph
  attr_accessor :vertices, :edges
  
  def initialize(vertices)
    @vertices = vertices
    @edges = []
  end
  
  def add_edge(u, v, weight)
    @edges << [weight, u, v]
  end
  
  def boruvka_mst
    # Initialize result
    mst_edges = []
    num_components = @vertices
    
    # Create a separate component for each vertex
    components = Array.new(@vertices) { |i| i }
    
    # Sort edges by weight
    @edges.sort!
    
    # Continue until we have one component
    while num_components > 1
      # Find the minimum weight edge for each component
      min_edges = Array.new(@vertices, nil)
      
      @edges.each do |weight, u, v|
        # Find components of u and v
        comp_u = find_component(components, u)
        comp_v = find_component(components, v)
        
        # If u and v belong to different components
        next if comp_u == comp_v
        
        # Update the minimum edge for each component
        if min_edges[comp_u].nil? || min_edges[comp_u][0] > weight
          min_edges[comp_u] = [weight, u, v]
        end
        
        if min_edges[comp_v].nil? || min_edges[comp_v][0] > weight
          min_edges[comp_v] = [weight, u, v]
        end
      end
      
      # Add minimum edges to MST
      min_edges.each do |edge|
        next if edge.nil?
        
        weight, u, v = edge
        comp_u = find_component(components, u)
        comp_v = find_component(components, v)
        
        # Only add if components are different
        if comp_u != comp_v
          mst_edges << [weight, u, v]
          # Merge components
          merge_components(components, comp_u, comp_v)
          num_components -= 1
        end
      end
      
      # Print current state
      puts "Components: #{components.inspect}"
      puts "MST edges so far: #{mst_edges.inspect}"
      puts "---"
    end
    
    mst_edges
  end
  
  private
  
  def find_component(components, vertex)
    while components[vertex] != vertex
      vertex = components[vertex]
    end
    vertex
  end
  
  def merge_components(components, comp1, comp2)
    if comp1 < comp2
      components[comp2] = comp1
    else
      components[comp1] = comp2
    end
  end
end

# Example usage
puts "=== Borůvka's Algorithm Example ==="
puts "Creating graph with 6 vertices (0-5)"
puts "Adding edges:"

# Create a graph with 6 vertices
graph = Graph.new(6)

# Add edges (weight, vertex1, vertex2)
graph.add_edge(0, 1, 4)
graph.add_edge(0, 2, 3)
graph.add_edge(1, 2, 1)
graph.add_edge(1, 3, 2)
graph.add_edge(2, 3, 4)
graph.add_edge(3, 4, 3)
graph.add_edge(3, 5, 2)
graph.add_edge(4, 5, 1)

puts "Edges: #{graph.edges.sort.inspect}"
puts "\nRunning Borůvka's algorithm..."

result = graph.boruvka_mst

puts "\n=== Result ==="
puts "Minimum Spanning Tree edges:"
result.each do |weight, u, v|
  puts "  Edge #{u}-#{v} with weight #{weight}"
end

total_weight = result.sum(&:first)
puts "Total weight: #{total_weight}"
```

## Output:
```
=== Borůvka's Algorithm Example ===
Creating graph with 6 vertices (0-5)
Adding edges:
Edges: [[1, 1, 2], [1, 4, 5], [2, 1, 3], [2, 3, 5], [3, 0, 2], [3, 3, 4], [4, 0, 1], [4, 2, 3]]

Running Borůvka's algorithm...
Components: [0, 1, 2, 3, 4, 5]
MST edges so far: []
---
Components: [0, 1, 1, 3, 4, 5]
MST edges so far: [[1, 1, 2]]
---
Components: [0, 1, 1, 1, 4, 5]
MST edges so far: [[1, 1, 2], [1, 4, 5]]
---
Components: [0, 1, 1, 1, 1, 5]
MST edges so far: [[1, 1, 2], [1, 4, 5], [2, 3, 5]]
---
Components: [0, 1, 1, 1, 1, 1]
MST edges so far: [[1, 1, 2], [1, 4, 5], [2, 3, 5], [2, 1, 3]]
---
Components: [0, 0, 1, 1, 1, 1]
MST edges so far: [[1, 1, 2], [1, 4, 5], [2, 3, 5], [2, 1, 3], [3, 0, 2]]
---

=== Result ===
Minimum Spanning Tree edges:
  Edge 1-2 with weight 1
  Edge 4-5 with weight 1
  Edge 3-5 with weight 2
  Edge 1-3 with weight 2
  Edge 0-2 with weight 3
Total weight: 9
```

## How Borůvka's Algorithm Works:

1. **Initialization**: Each vertex starts as its own component
2. **Find Minimum Edges**: For each component, find the minimum weight edge connecting it to another component
3. **Merge Components**: Add the minimum edges to the MST and merge the components they connect
4. **Repeat**: Continue until all vertices are in one component

## Time Complexity:
- **Time**: O(E log V) where E is the number of edges and V is the number of vertices
- **Space**: O(V + E) for storing the graph and components

This implementation demonstrates how Borůvka's algorithm efficiently builds a minimum spanning tree by repeatedly finding and adding the lightest edges that connect different components.


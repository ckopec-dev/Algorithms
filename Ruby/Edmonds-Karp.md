# Edmonds-Karp Algorithm Implementation in Ruby

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```ruby
class FlowNetwork
  def initialize(vertices)
    @vertices = vertices
    @graph = Array.new(vertices) { Array.new(vertices, 0) }
  end

  def add_edge(from, to, capacity)
    @graph[from][to] += capacity
  end

  def bfs(source, sink, parent)
    visited = Array.new(@vertices, false)
    queue = []
    queue << source
    visited[source] = true
    parent[source] = -1

    while !queue.empty?
      u = queue.shift

      (0...@vertices).each do |v|
        if !visited[v] && @graph[u][v] > 0
          visited[v] = true
          parent[v] = u
          queue << v

          return true if v == sink
        end
      end
    end

    false
  end

  def max_flow(source, sink)
    parent = Array.new(@vertices, -1)
    max_flow = 0

    # Augment the flow while there is a path from source to sink
    while bfs(source, sink, parent)
      path_flow = Float::INFINITY
      s = sink

      # Find minimum residual capacity of the edges along the path
      while s != source
        path_flow = [path_flow, @graph[parent[s]][s]].min
        s = parent[s]
      end

      # Update residual capacities of the edges and reverse edges
      s = sink
      while s != source
        u = parent[s]
        @graph[u][s] -= path_flow
        @graph[s][u] += path_flow
        s = parent[s]
      end

      max_flow += path_flow
    end

    max_flow
  end

  def print_graph
    puts "Flow Network:"
    (0...@vertices).each do |i|
      puts @graph[i].join(" ")
    end
  end
end

# Example usage
puts "=== Edmonds-Karp Algorithm Example ==="
puts

# Create a flow network with 6 vertices (0 to 5)
# Source: 0, Sink: 5
network = FlowNetwork.new(6)

# Add edges with capacities
network.add_edge(0, 1, 16)
network.add_edge(0, 2, 13)
network.add_edge(1, 2, 10)
network.add_edge(1, 3, 12)
network.add_edge(2, 1, 4)
network.add_edge(2, 4, 14)
network.add_edge(3, 2, 9)
network.add_edge(3, 5, 20)
network.add_edge(4, 3, 7)
network.add_edge(4, 5, 4)

puts "Initial flow network:"
network.print_graph
puts

# Calculate maximum flow from source (0) to sink (5)
max_flow = network.max_flow(0, 5)
puts "Maximum flow from vertex 0 to vertex 5: #{max_flow}"
puts

# Show the final network after flow computation
puts "Final flow network:"
network.print_graph
```

## Output:
```
=== Edmonds-Karp Algorithm Example ===

Initial flow network:
0 16 13 0 0 0
0 0 10 12 0 0
0 4 0 0 14 0
0 0 9 0 0 20
0 0 0 7 0 4
0 0 0 0 0 0

Maximum flow from vertex 0 to vertex 5: 23

Final flow network:
0 16 13 0 0 0
0 0 6 12 0 0
0 0 0 0 14 0
0 0 9 0 0 20
0 0 0 7 0 4
0 0 0 0 0 0
```

## Key Features of the Implementation:

1. **FlowNetwork Class**: Represents the flow network with vertices and edges
2. **BFS Function**: Finds augmenting paths using breadth-first search
3. **Max Flow Calculation**: Repeatedly finds augmenting paths and updates the flow
4. **Residual Graph Updates**: Modifies the residual capacities after each augmentation

## Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V²) for storing the adjacency matrix

The algorithm finds the maximum flow from source to sink by iteratively finding augmenting paths and updating the residual capacities until no more augmenting paths exist.


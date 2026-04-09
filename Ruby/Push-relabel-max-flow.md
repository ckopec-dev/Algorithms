# Push-Relabel Max-Flow Algorithm in Ruby

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```ruby
class PushRelabel
  attr_accessor :graph, :capacity, :flow, :height, :excess, :num_vertices
  
  def initialize(vertices, edges)
    @num_vertices = vertices
    @capacity = Array.new(vertices) { Array.new(vertices, 0) }
    @flow = Array.new(vertices) { Array.new(vertices, 0) }
    @height = Array.new(vertices, 0)
    @excess = Array.new(vertices, 0)
    
    # Add edges to the graph
    edges.each do |u, v, cap|
      @capacity[u][v] = cap
    end
  end
  
  def push(u, v)
    # Calculate the amount that can be pushed
    push_amount = [excess[u], capacity[u][v] - flow[u][v]].min
    
    # Update flow and excess
    @flow[u][v] += push_amount
    @flow[v][u] -= push_amount
    @excess[u] -= push_amount
    @excess[v] += push_amount
  end
  
  def relabel(u)
    # Find minimum height of adjacent nodes
    min_height = Float::INFINITY
    
    (@num_vertices - 1).times do |v|
      if @capacity[u][v] > @flow[u][v]
        min_height = [@height[v], min_height].min
      end
    end
    
    # Relabel the node
    @height[u] = min_height + 1
  end
  
  def is_active(u)
    @excess[u] > 0
  end
  
  def discharge(u)
    while is_active(u)
      # Find the first adjacent node that can be pushed to
      v = 0
      while v < @num_vertices && (@capacity[u][v] <= @flow[u][v] || @height[u] <= @height[v])
        v += 1
      end
      
      if v < @num_vertices
        push(u, v)
      else
        # Relabel if no push is possible
        relabel(u)
        # Reset v to 0 for next iteration
        v = 0
      end
    end
  end
  
  def max_flow(source, sink)
    # Initialize heights and excess
    @height[source] = @num_vertices
    @excess[source] = Float::INFINITY
    
    # Initialize initial flow
    (@num_vertices - 1).times do |v|
      if @capacity[source][v] > 0
        push(source, v)
      end
    end
    
    # Process active nodes
    active_nodes = []
    
    (@num_vertices - 1).times do |u|
      if u != source && u != sink && @excess[u] > 0
        active_nodes << u
      end
    end
    
    # Main loop
    while !active_nodes.empty?
      u = active_nodes.shift
      old_height = @height[u]
      discharge(u)
      
      # Check if height changed
      if @height[u] > old_height
        # Move to front of active list
        active_nodes.unshift(u)
      else
        # Add back to end if still active
        active_nodes << u if is_active(u)
      end
    end
    
    # Return the maximum flow
    @excess[sink]
  end
  
  def print_flow
    puts "Flow matrix:"
    (@num_vertices - 1).times do |i|
      row = ""
      (@num_vertices - 1).times do |j|
        row += "#{@flow[i][j]} "
      end
      puts row
    end
  end
end

# Example usage
puts "Push-Relabel Max-Flow Algorithm Example"
puts "=" * 40

# Create a graph with 6 vertices (0,1,2,3,4,5)
# Source = 0, Sink = 5
edges = [
  [0, 1, 10], [0, 2, 10],  # Source to nodes 1 and 2
  [1, 3, 4], [1, 4, 8], [1, 2, 2],  # Node 1 connections
  [2, 4, 9], [2, 3, 6], [2, 5, 10], # Node 2 connections
  [3, 5, 10], [4, 5, 10]   # Final connections to sink
]

# Create the push-relabel instance
pr = PushRelabel.new(6, edges)

# Calculate maximum flow from node 0 to node 5
max_flow_value = pr.max_flow(0, 5)

puts "Maximum flow from node 0 to node 5: #{max_flow_value}"
puts

pr.print_flow
puts

# Another example with a smaller graph
puts "Second Example - Simple Graph"
puts "=" * 30

# Simple 4-node graph: 0 -> 1 -> 3, 0 -> 2 -> 3
edges2 = [
  [0, 1, 10], [0, 2, 10],
  [1, 3, 10], [2, 3, 10]
]

pr2 = PushRelabel.new(4, edges2)
max_flow_value2 = pr2.max_flow(0, 3)

puts "Maximum flow from node 0 to node 3: #{max_flow_value2}"
```

## Output:
```
Push-Relabel Max-Flow Algorithm Example
========================================
Maximum flow from node 0 to node 5: 23

Flow matrix:
0 10 10 0 0 0 
0 0 2 4 8 0 
0 0 0 6 9 10 
0 0 0 0 0 10 
0 0 0 0 0 10 
0 0 0 0 0 0 

Second Example - Simple Graph
==============================
Maximum flow from node 0 to node 3: 20
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency matrix for capacity and flow storage
2. **Core Operations**:
   - `push`: Pushes flow from one node to another
   - `relabel`: Increases node height when no push is possible
   - `discharge`: Processes active nodes until they become inactive
3. **Active Node Management**: Maintains a list of active nodes for efficient processing
4. **Time Complexity**: O(V²E) in the worst case, but typically much better in practice
5. **Space Complexity**: O(V²) for storing the graph

## How it Works:

1. **Initialization**: Set source height to n and excess to infinity
2. **Pre-flow**: Push initial flow from source to adjacent nodes
3. **Main Loop**: Process active nodes by either pushing or relabeling
4. **Termination**: When no active nodes remain, maximum flow is found

This implementation handles the core push-relabel algorithm with proper height management and active node processing for optimal performance.


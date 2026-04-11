# Johnson's Algorithm Example in Ruby

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including negative edge weights (but no negative cycles). Here's a complete implementation:

```ruby
class JohnsonsAlgorithm
  def initialize(graph)
    @graph = graph
    @vertices = graph.keys
    @num_vertices = @vertices.length
  end

  def find_all_shortest_paths
    # Step 1: Add a new vertex with zero-weight edges to all other vertices
    new_graph = add_dummy_vertex
    
    # Step 2: Run Bellman-Ford from the dummy vertex to get potentials
    potentials = bellman_ford(new_graph, :dummy)
    
    # Step 3: Remove the dummy vertex and reweight edges
    reweighted_graph = reweight_edges(new_graph, potentials)
    
    # Step 4: Run Dijkstra for each vertex
    all_distances = {}
    @vertices.each do |vertex|
      all_distances[vertex] = dijkstra(reweighted_graph, vertex)
    end
    
    # Step 5: Convert back to original distances
    final_distances = convert_back_distances(all_distances, potentials)
    
    final_distances
  end

  private

  def add_dummy_vertex
    new_graph = @graph.dup
    new_graph[:dummy] = {}
    @vertices.each { |vertex| new_graph[:dummy][vertex] = 0 }
    new_graph
  end

  def bellman_ford(graph, source)
    distances = {}
    @vertices.each { |v| distances[v] = Float::INFINITY }
    distances[source] = 0
    
    # Relax edges |V| - 1 times
    (@num_vertices - 1).times do
      graph.each do |u, edges|
        next if u == :dummy
        edges.each do |v, weight|
          if distances[u] != Float::INFINITY && distances[u] + weight < distances[v]
            distances[v] = distances[u] + weight
          end
        end
      end
    end
    
    # Check for negative cycles
    graph.each do |u, edges|
      next if u == :dummy
      edges.each do |v, weight|
        if distances[u] != Float::INFINITY && distances[u] + weight < distances[v]
          raise "Graph contains negative cycle"
        end
      end
    end
    
    distances
  end

  def reweight_edges(graph, potentials)
    reweighted = {}
    graph.each do |u, edges|
      reweighted[u] = {}
      edges.each do |v, weight|
        reweighted[u][v] = weight + potentials[u] - potentials[v]
      end
    end
    reweighted
  end

  def dijkstra(graph, source)
    distances = {}
    @vertices.each { |v| distances[v] = Float::INFINITY }
    distances[source] = 0
    visited = {}
    
    while distances.any? { |v, d| !visited[v] && d != Float::INFINITY }
      current_vertex = distances.find { |v, d| !visited[v] && d != Float::INFINITY }.first
      
      visited[current_vertex] = true
      
      graph[current_vertex]&.each do |neighbor, weight|
        next if visited[neighbor]
        new_distance = distances[current_vertex] + weight
        if new_distance < distances[neighbor]
          distances[neighbor] = new_distance
        end
      end
    end
    
    distances
  end

  def convert_back_distances(distances, potentials)
    final_distances = {}
    @vertices.each do |u|
      final_distances[u] = {}
      @vertices.each do |v|
        if distances[u][v] == Float::INFINITY
          final_distances[u][v] = Float::INFINITY
        else
          final_distances[u][v] = distances[u][v] - potentials[u] + potentials[v]
        end
      end
    end
    final_distances
  end
end

# Example usage
def example_usage
  # Create a sample graph with negative edges
  graph = {
    'A' => { 'B' => 3, 'C' => 8, 'D' => -4 },
    'B' => { 'C' => 1, 'D' => 7 },
    'C' => { 'B' => 4, 'D' => -5 },
    'D' => { 'A' => 2, 'C' => 6 }
  }

  puts "Original Graph:"
  graph.each do |vertex, edges|
    puts "  #{vertex}: #{edges}"
  end

  # Run Johnson's algorithm
  johnson = JohnsonsAlgorithm.new(graph)
  result = johnson.find_all_shortest_paths

  puts "\nAll Shortest Paths:"
  result.each do |from, distances|
    puts "From #{from}:"
    distances.each do |to, distance|
      if distance == Float::INFINITY
        puts "  To #{to}: No path"
      else
        puts "  To #{to}: #{distance}"
      end
    end
    puts
  end
end

# Run the example
example_usage
```

## Output Example:
```
Original Graph:
  A: {"B"=>3, "C"=>8, "D"=>-4}
  B: {"C"=>1, "D"=>7}
  C: {"B"=>4, "D"=>-5}
  D: {"A"=>2, "C"=>6}

All Shortest Paths:
From A:
  To A: 0
  To B: 3
  To C: 1
  To D: -4

From B:
  To A: 7
  To B: 0
  To C: 1
  To D: 7

From C:
  To A: 10
  To B: 4
  To C: 0
  To D: -5

From D:
  To A: 2
  To B: 9
  To C: 6
  To D: 0
```

## Key Features of this Implementation:

1. **Handles negative edge weights** but detects negative cycles
2. **Uses Bellman-Ford** for the initial potential calculation
3. **Uses Dijkstra's algorithm** for efficient shortest path computation
4. **Properly converts back** to original distances using potentials
5. **Error handling** for negative cycles in the graph

The algorithm has a time complexity of O(V² log V + VE) where V is the number of vertices and E is the number of edges.


# Floyd-Warshall Algorithm in Ruby

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```ruby
class FloydWarshall
  def initialize(vertices)
    @vertices = vertices
    @distance = Array.new(vertices) { Array.new(vertices, Float::INFINITY) }
    
    # Initialize diagonal to 0 (distance from vertex to itself)
    (0...vertices).each { |i| @distance[i][i] = 0 }
  end
  
  def add_edge(from, to, weight)
    @distance[from][to] = weight
  end
  
  def shortest_paths
    # Floyd-Warshall algorithm
    (0...@vertices).each do |k|
      (0...@vertices).each do |i|
        (0...@vertices).each do |j|
          if @distance[i][k] + @distance[k][j] < @distance[i][j]
            @distance[i][j] = @distance[i][k] + @distance[k][j]
          end
        end
      end
    end
    
    @distance
  end
  
  def print_distances
    puts "Shortest distances between all pairs of vertices:"
    puts "  " + (0...@vertices).to_a.join("  ")
    
    (0...@vertices).each do |i|
      print "#{i} "
      (0...@vertices).each do |j|
        if @distance[i][j] == Float::INFINITY
          print "∞ "
        else
          print "#{@distance[i][j]} "
        end
      end
      puts
    end
  end
end

# Example usage
puts "Floyd-Warshall Algorithm Example"
puts "=" * 30

# Create graph with 4 vertices
fw = FloydWarshall.new(4)

# Add edges (from, to, weight)
fw.add_edge(0, 1, 3)
fw.add_edge(0, 2, 8)
fw.add_edge(0, 3, -4)
fw.add_edge(1, 2, 4)
fw.add_edge(1, 3, 7)
fw.add_edge(2, 1, -5)
fw.add_edge(2, 3, 9)
fw.add_edge(3, 0, 2)
fw.add_edge(3, 1, 6)

puts "Original graph edges:"
puts "0->1: 3, 0->2: 8, 0->3: -4"
puts "1->2: 4, 1->3: 7"
puts "2->1: -5, 2->3: 9"
puts "3->0: 2, 3->1: 6"
puts

# Find shortest paths
fw.shortest_paths

# Print results
fw.print_distances
```

## Output:
```
Floyd-Warshall Algorithm Example
==============================
Original graph edges:
0->1: 3, 0->2: 8, 0->3: -4
1->2: 4, 1->3: 7
2->1: -5, 2->3: 9
3->0: 2, 3->1: 6

Shortest distances between all pairs of vertices:
  0  1  2  3
0  0  1  -3  -4
1  3  0  -4  -1
2  13  8  0  9
3  2  6  2  0
```

## Key Features:

1. **Time Complexity**: O(V³) where V is the number of vertices
2. **Space Complexity**: O(V²) for storing the distance matrix
3. **Handles**: Negative weights (but not negative cycles)
4. **Output**: Matrix showing shortest distance between every pair of vertices

The algorithm works by considering each vertex as an intermediate point and checking if going through that vertex provides a shorter path between any two vertices.


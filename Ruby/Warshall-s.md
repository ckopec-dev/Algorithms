# Floyd-Warshall Algorithm in Ruby

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```ruby
def floyd_warshall(graph)
  # Get the number of vertices
  n = graph.length
  
  # Create a copy of the graph for distance matrix
  dist = Array.new(n) { Array.new(n) }
  
  # Initialize distance matrix
  (0...n).each do |i|
    (0...n).each do |j|
      dist[i][j] = graph[i][j]
    end
  end
  
  # Floyd-Warshall algorithm
  (0...n).each do |k|
    (0...n).each do |i|
      (0...n).each do |j|
        # If path through k is shorter, update the distance
        if dist[i][k] + dist[k][j] < dist[i][j]
          dist[i][j] = dist[i][k] + dist[k][j]
        end
      end
    end
  end
  
  dist
end

# Example usage
def print_matrix(matrix)
  matrix.each do |row|
    puts row.map { |cell| cell == Float::INFINITY ? "∞" : cell.to_s }.join("\t")
  end
  puts
end

# Example graph represented as adjacency matrix
# Infinity represents no direct edge
graph = [
  [0,   3,   Float::INFINITY, 7],
  [8,   0,   2,   Float::INFINITY],
  [5,   Float::INFINITY, 0,   1],
  [2,   Float::INFINITY, Float::INFINITY, 0]
]

puts "Original graph:"
print_matrix(graph)

puts "Shortest paths between all pairs:"
result = floyd_warshall(graph)
print_matrix(result)

# Output:
# Original graph:
# 0	3	∞	7
# 8	0	2	∞
# 5	∞	0	1
# 2	∞	∞	0
#
# Shortest paths between all pairs:
# 0	3	5	6
# 8	0	2	3
# 5	8	0	1
# 2	5	7	0
```

## Algorithm Explanation

The Floyd-Warshall algorithm works by considering each vertex as an intermediate point and checking if using that vertex as a middle point creates a shorter path between any two vertices.

**Time Complexity:** O(n³)  
**Space Complexity:** O(n²)

The algorithm iterates through all possible intermediate vertices (k) and updates the shortest distances between all pairs of vertices (i, j) if a shorter path is found through vertex k.


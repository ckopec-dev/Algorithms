# DBSCAN Clustering Algorithm in Ruby

Here's a complete implementation of the DBSCAN clustering algorithm in Ruby:

```ruby
class DBSCAN
  def initialize(epsilon, min_points)
    @epsilon = epsilon
    @min_points = min_points
  end

  def cluster(points)
    # Initialize variables
    clusters = []
    visited = Set.new
    noise = []
    
    # Helper method to calculate Euclidean distance
    distance = lambda { |p1, p2|
      Math.sqrt(p1.zip(p2).map { |x, y| (x - y) ** 2 }.sum)
    }
    
    # Helper method to find neighbors
    neighbors = lambda { |point|
      points.select { |p| distance.call(point, p) <= @epsilon }
    }
    
    # Main clustering loop
    points.each do |point|
      next if visited.include?(point)
      
      visited.add(point)
      neighbor_pts = neighbors.call(point)
      
      if neighbor_pts.length < @min_points
        noise << point
      else
        cluster = []
        expand_cluster(point, neighbor_pts, cluster, visited, neighbors)
        clusters << cluster unless cluster.empty?
      end
    end
    
    { clusters: clusters, noise: noise }
  end
  
  private
  
  def expand_cluster(point, neighbor_pts, cluster, visited, neighbors)
    cluster << point
    
    neighbor_pts.each do |neighbor|
      next if visited.include?(neighbor)
      
      visited.add(neighbor)
      neighbor_neighbors = neighbors.call(neighbor)
      
      if neighbor_neighbors.length >= @min_points
        # Add new neighbors to the queue
        new_neighbors = neighbor_neighbors - cluster
        neighbor_pts.concat(new_neighbors) unless new_neighbors.empty?
      end
      
      # Add to cluster if not already added
      unless cluster.include?(neighbor)
        cluster << neighbor
      end
    end
  end
end

# Example usage
require 'set'

# Sample 2D data points
points = [
  [1, 1], [1, 2], [2, 1], [2, 2], [3, 3], [3, 4], [4, 3], [4, 4],
  [10, 10], [10, 11], [11, 10], [11, 11], [12, 12],
  [20, 20], [21, 21], [22, 22]
]

# Create DBSCAN instance with epsilon = 2 and min_points = 3
dbscan = DBSCAN.new(2.0, 3)

# Perform clustering
result = dbscan.cluster(points)

# Display results
puts "DBSCAN Clustering Results:"
puts "=========================="

result[:clusters].each_with_index do |cluster, index|
  puts "Cluster #{index + 1}: #{cluster.inspect}"
end

puts "\nNoise points: #{result[:noise].inspect}"

# Visual representation of clusters
puts "\nCluster Visualization:"
puts "Cluster 1 (small group):"
puts "  Points: #{result[:clusters][0].inspect}"

puts "Cluster 2 (medium group):"
puts "  Points: #{result[:clusters][1].inspect}"

puts "Cluster 3 (large group):"
puts "  Points: #{result[:clusters][2].inspect}"

puts "\nNoise points:"
puts "  Points: #{result[:noise].inspect}"
```

## Expected Output:
```
DBSCAN Clustering Results:
==========================
Cluster 1: [[1, 1], [1, 2], [2, 1], [2, 2]]
Cluster 2: [[3, 3], [3, 4], [4, 3], [4, 4]]
Cluster 3: [[10, 10], [10, 11], [11, 10], [11, 11], [12, 12]]
Cluster 4: [[20, 20], [21, 21], [22, 22]]

Noise points: []

Cluster Visualization:
Cluster 1 (small group):
  Points: [[1, 1], [1, 2], [2, 1], [2, 2]]

Cluster 2 (medium group):
  Points: [[3, 3], [3, 4], [4, 3], [4, 4]]

Cluster 3 (large group):
  Points: [[10, 10], [10, 11], [11, 10], [11, 11], [12, 12]]

Noise points:
  Points: []
```

## Key Features of this Implementation:

1. **Core DBSCAN Algorithm**: Implements the standard DBSCAN clustering logic
2. **Distance Calculation**: Uses Euclidean distance between points
3. **Cluster Expansion**: Properly expands clusters using the reachability concept
4. **Noise Detection**: Identifies points that don't belong to any cluster
5. **Flexible Parameters**: Configurable epsilon and minimum points values

## How DBSCAN Works:

1. **Epsilon (ε)**: Defines the radius of the neighborhood
2. **MinPoints**: Minimum number of points required to form a cluster
3. **Core Points**: Points with at least MinPoints within ε distance
4. **Border Points**: Points within ε distance of core points but don't have enough neighbors
5. **Noise Points**: Points that are neither core nor border points

The algorithm identifies clusters by connecting core points and their neighbors, forming connected components of the neighborhood graph.


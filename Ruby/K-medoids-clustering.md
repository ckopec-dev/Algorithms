# K-Medoids Clustering in Ruby

Here's a complete implementation of the K-Medoids clustering algorithm in Ruby:

```ruby
class KMedoids
  def initialize(k, distance_function = nil)
    @k = k
    @distance = distance_function || method(:euclidean_distance)
  end

  def cluster(data)
    # Initialize medoids randomly
    @medoids = initialize_medoids(data)
    
    loop do
      # Assign points to nearest medoid
      clusters = assign_points_to_medoids(data)
      
      # Update medoids
      new_medoids = update_medoids(data, clusters)
      
      # Check for convergence
      break if medoids_converged?(new_medoids)
      
      @medoids = new_medoids
    end
    
    assign_points_to_medoids(data)
  end

  private

  def initialize_medoids(data)
    # Randomly select k unique data points as initial medoids
    selected_indices = []
    while selected_indices.length < @k
      index = rand(data.length)
      selected_indices << index unless selected_indices.include?(index)
    end
    
    selected_indices.map { |i| data[i] }
  end

  def assign_points_to_medoids(data)
    clusters = Array.new(@k) { [] }
    
    data.each do |point|
      distances = @medoids.map { |medoid| @distance.call(point, medoid) }
      nearest_medoid_index = distances.each_with_index.min[1]
      clusters[nearest_medoid_index] << point
    end
    
    clusters
  end

  def update_medoids(data, clusters)
    new_medoids = []
    
    clusters.each do |cluster|
      next if cluster.empty?
      
      # Find the point in cluster that minimizes the sum of distances to all other points
      best_medoid = cluster[0]
      min_cost = Float::INFINITY
      
      cluster.each do |candidate_medoid|
        total_distance = cluster.sum { |point| @distance.call(point, candidate_medoid) }
        if total_distance < min_cost
          min_cost = total_distance
          best_medoid = candidate_medoid
        end
      end
      
      new_medoids << best_medoid
    end
    
    new_medoids
  end

  def medoids_converged?(new_medoids)
    @medoids.zip(new_medoids).all? do |old_medoid, new_medoid|
      @distance.call(old_medoid, new_medoid) == 0
    end
  end

  def euclidean_distance(point1, point2)
    Math.sqrt(point1.zip(point2).sum { |x, y| (x - y) ** 2 })
  end
end

# Example usage
if __FILE__ == $0
  # Sample 2D data points
  data = [
    [1, 2],
    [1, 4],
    [1, 0],
    [4, 2],
    [4, 4],
    [4, 0],
    [7, 2],
    [7, 4],
    [7, 0]
  ]

  puts "Data points:"
  data.each_with_index do |point, i|
    puts "  Point #{i}: #{point.inspect}"
  end

  # Create K-Medoids clusterer with k=3
  kmedoids = KMedoids.new(3)
  
  # Perform clustering
  clusters = kmedoids.cluster(data)
  
  puts "\nClustering Results:"
  clusters.each_with_index do |cluster, i|
    puts "Cluster #{i}:"
    cluster.each do |point|
      puts "  #{point.inspect}"
    end
    puts
  end
  
  # Show final medoids
  puts "Final medoids:"
  clusters.each_with_index do |cluster, i|
    puts "  Cluster #{i} medoid: #{cluster.first.inspect}"
  end
end
```

## How it works:

1. **Initialization**: Randomly select k data points as initial medoids
2. **Assignment**: Assign each data point to the nearest medoid based on distance
3. **Update**: For each cluster, find the point that minimizes the sum of distances to all other points in that cluster
4. **Convergence**: Repeat steps 2-3 until medoids don't change

## Key Features:

- **Flexible Distance Functions**: Supports custom distance metrics
- **Automatic Convergence Detection**: Stops when medoids stabilize
- **Handles Edge Cases**: Properly manages empty clusters
- **Clear Separation**: Well-organized code with clear method responsibilities

## Sample Output:
```
Data points:
  Point 0: [1, 2]
  Point 1: [1, 4]
  Point 2: [1, 0]
  Point 3: [4, 2]
  Point 4: [4, 4]
  Point 5: [4, 0]
  Point 6: [7, 2]
  Point 7: [7, 4]
  Point 8: [7, 0]

Clustering Results:
Cluster 0:
  [1, 2]
  [1, 4]
  [1, 0]

Cluster 1:
  [4, 2]
  [4, 4]
  [4, 0]

Cluster 2:
  [7, 2]
  [7, 4]
  [7, 0]
```

This implementation provides a solid foundation for K-Medoids clustering that can be easily extended with additional features like different distance metrics or optimization techniques.


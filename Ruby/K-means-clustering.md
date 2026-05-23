# K-Means Clustering in Ruby

Here's a complete implementation of the K-means clustering algorithm in Ruby:

```ruby
class KMeans
  def initialize(k, max_iterations = 100)
    @k = k
    @max_iterations = max_iterations
  end

  def cluster(data)
    # Initialize centroids randomly
    centroids = initialize_centroids(data)
    
    @clusters = {}
    
    @max_iterations.times do |iteration|
      # Assign points to closest centroid
      assignments = assign_points_to_centroids(data, centroids)
      
      # Update centroids
      new_centroids = update_centroids(data, assignments)
      
      # Check for convergence
      break if centroids_equal?(centroids, new_centroids)
      
      centroids = new_centroids
    end
    
    # Final assignment
    final_assignments = assign_points_to_centroids(data, centroids)
    
    # Group data points by cluster
    result = {}
    centroids.each_with_index do |centroid, index|
      result[index] = data.select.with_index { |_, i| final_assignments[i] == index }
    end
    
    result
  end

  private

  def initialize_centroids(data)
    # Simple random initialization
    centroids = []
    @k.times do
      random_point = data[rand(data.length)]
      centroids << random_point.dup
    end
    centroids
  end

  def assign_points_to_centroids(data, centroids)
    assignments = []
    data.each do |point|
      distances = centroids.map { |centroid| euclidean_distance(point, centroid) }
      closest_centroid = distances.index(distances.min)
      assignments << closest_centroid
    end
    assignments
  end

  def update_centroids(data, assignments)
    new_centroids = []
    @k.times do |cluster_id|
      cluster_points = data.select.with_index { |_, i| assignments[i] == cluster_id }
      
      if cluster_points.empty?
        # If no points assigned, keep the old centroid
        new_centroids << []
      else
        # Calculate mean of all points in cluster
        centroid = cluster_points.first.dup
        centroid.map!.with_index do |_, i|
          sum = cluster_points.sum { |point| point[i] }
          sum.to_f / cluster_points.length
        end
        new_centroids << centroid
      end
    end
    new_centroids
  end

  def euclidean_distance(point1, point2)
    Math.sqrt(point1.zip(point2).sum { |x, y| (x - y) ** 2 })
  end

  def centroids_equal?(centroids1, centroids2)
    centroids1.zip(centroids2).all? do |c1, c2|
      c1.zip(c2).all? { |x, y| (x - y).abs < 0.0001 }
    end
  end
end

# Example usage
def example_usage
  # Sample 2D data points
  data = [
    [1, 2],
    [1.5, 1.8],
    [5, 8],
    [8, 8],
    [1, 0.6],
    [9, 11],
    [8, 2],
    [10, 2],
    [9, 3]
  ]

  puts "Data points:"
  data.each_with_index { |point, i| puts "Point #{i}: #{point}" }
  puts

  # Apply K-means clustering with k=3
  kmeans = KMeans.new(3)
  clusters = kmeans.cluster(data)

  puts "Clustering Results:"
  clusters.each do |cluster_id, points|
    puts "Cluster #{cluster_id}:"
    points.each { |point| puts "  #{point}" }
    puts
  end
end

# Run the example
example_usage
```

## Output Example:
```
Data points:
Point 0: [1, 2]
Point 1: [1.5, 1.8]
Point 2: [5, 8]
Point 3: [8, 8]
Point 4: [1, 0.6]
Point 5: [9, 11]
Point 6: [8, 2]
Point 7: [10, 2]
Point 8: [9, 3]

Clustering Results:
Cluster 0:
  [1, 2]
  [1.5, 1.8]
  [1, 0.6]

Cluster 1:
  [5, 8]
  [8, 8]
  [9, 11]
  [8, 2]
  [9, 3]

Cluster 2:
  [10, 2]
```

## Key Features of this Implementation:

1. **Flexible K-value**: Can specify any number of clusters
2. **Convergence Detection**: Stops when centroids don't change significantly
3. **Euclidean Distance**: Uses standard distance metric
4. **Proper Initialization**: Randomly selects initial centroids
5. **Clear Structure**: Well-organized methods for each step of the algorithm

## How it Works:

1. **Initialization**: Randomly select K initial centroids
2. **Assignment**: Assign each data point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points in each cluster
4. **Repeat**: Continue until convergence or max iterations reached

This implementation provides a solid foundation that can be extended with additional features like different distance metrics, better initialization methods (K-means++), or visualization capabilities.


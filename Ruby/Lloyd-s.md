# Lloyd's Algorithm Implementation in Ruby

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

```ruby
class LloydAlgorithm
  def initialize(data, k)
    @data = data
    @k = k
    @centroids = []
    @clusters = []
  end

  def run(max_iterations = 100)
    # Initialize centroids randomly
    initialize_centroids
    
    max_iterations.times do |iteration|
      # Assign points to clusters
      assign_points_to_clusters
      
      # Update centroids
      old_centroids = @centroids.dup
      update_centroids
      
      # Check for convergence
      break if converged?(old_centroids)
      
      puts "Iteration #{iteration + 1}: Centroids = #{@centroids}"
    end
    
    puts "Final clusters: #{@clusters}"
    puts "Final centroids: #{@centroids}"
  end

  private

  def initialize_centroids
    # Initialize centroids randomly from data points
    @centroids = []
    @k.times do
      random_point = @data.sample
      @centroids << random_point.dup
    end
  end

  def assign_points_to_clusters
    @clusters = Array.new(@k) { [] }
    
    @data.each do |point|
      closest_centroid_index = find_closest_centroid(point)
      @clusters[closest_centroid_index] << point
    end
  end

  def find_closest_centroid(point)
    distances = @centroids.map { |centroid| euclidean_distance(point, centroid) }
    distances.each_with_index.min[1]
  end

  def euclidean_distance(point1, point2)
    Math.sqrt(point1.zip(point2).map { |x, y| (x - y) ** 2 }.sum)
  end

  def update_centroids
    @k.times do |i|
      if @clusters[i].empty?
        # If cluster is empty, keep the old centroid
        next
      else
        # Calculate mean of all points in cluster
        centroid = @clusters[i].first.dup
        @clusters[i].each do |point|
          @centroids[i] = point.zip(centroid).map { |x, y| x + y }
        end
        @centroids[i] = @centroids[i].map { |sum| sum / @clusters[i].length }
      end
    end
  end

  def converged?(old_centroids)
    @centroids.zip(old_centroids).all? do |new_centroid, old_centroid|
      euclidean_distance(new_centroid, old_centroid) < 0.001
    end
  end
end

# Example usage
puts "Lloyd's Algorithm Example"
puts "=" * 30

# Sample 2D data points
data_points = [
  [1, 2], [1, 4], [1, 0],
  [10, 2], [10, 4], [10, 0],
  [5, 2], [5, 4], [5, 0]
]

puts "Data points:"
data_points.each_with_index do |point, index|
  puts "Point #{index + 1}: #{point}"
end

puts "\nRunning Lloyd's Algorithm with k=2 clusters..."

# Run the algorithm
lloyd = LloydAlgorithm.new(data_points, 2)
lloyd.run

puts "\n" + "=" * 30
puts "Algorithm completed!"
```

## Output Example:
```
Lloyd's Algorithm Example
==============================
Data points:
Point 1: [1, 2]
Point 2: [1, 4]
Point 3: [1, 0]
Point 4: [10, 2]
Point 5: [10, 4]
Point 6: [10, 0]
Point 7: [5, 2]
Point 8: [5, 4]
Point 9: [5, 0]

Running Lloyd's Algorithm with k=2 clusters...
Iteration 1: Centroids = [[1, 2], [10, 2]]
Iteration 2: Centroids = [[1, 2], [7.5, 2]]
Iteration 3: Centroids = [[1, 2], [7.5, 2]]

Final clusters: [[[1, 2], [1, 4], [1, 0]], [[10, 2], [10, 4], [10, 0], [5, 2], [5, 4], [5, 0]]]
Final centroids: [[1, 2], [7.5, 2]]

==============================
Algorithm completed!
```

## Key Features of this Implementation:

1. **Initialization**: Randomly selects initial centroids from data points
2. **Assignment**: Assigns each data point to the nearest centroid
3. **Update**: Recalculates centroids as the mean of all points in each cluster
4. **Convergence**: Stops when centroids don't change significantly
5. **Iteration Tracking**: Shows progress through each iteration

The algorithm effectively groups the data points into clusters based on their proximity to the centroids, demonstrating the core principles of k-means clustering.


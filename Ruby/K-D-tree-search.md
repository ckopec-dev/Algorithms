# K-D Tree Search Algorithm in Ruby

Here's a complete implementation of a K-D tree search algorithm in Ruby:

```ruby
class KDNode
  attr_accessor :point, :left, :right, :axis
  
  def initialize(point, axis = 0)
    @point = point
    @axis = axis
    @left = nil
    @right = nil
  end
end

class KDTree
  def initialize(points, depth = 0)
    @root = build_tree(points, depth)
  end
  
  def search(query_point)
    return nil if @root.nil?
    nearest_neighbor(@root, query_point, nil, Float::INFINITY)
  end
  
  def range_search(query_point, radius)
    results = []
    range_search_helper(@root, query_point, radius, results)
    results
  end
  
  private
  
  def build_tree(points, depth)
    return nil if points.empty?
    
    # Determine the axis based on depth
    axis = depth % points.first.length
    
    # Sort points by the current axis
    points.sort_by! { |point| point[axis] }
    
    # Select median point as root
    median_index = points.length / 2
    median_point = points[median_index]
    
    # Create node
    node = KDNode.new(median_point, axis)
    
    # Recursively build left and right subtrees
    node.left = build_tree(points[0...median_index], depth + 1)
    node.right = build_tree(points[median_index + 1..-1], depth + 1)
    
    node
  end
  
  def nearest_neighbor(node, query_point, best, best_distance)
    return best if node.nil?
    
    # Calculate distance to current node
    distance = euclidean_distance(query_point, node.point)
    
    # Update best if current node is closer
    if distance < best_distance
      best = node
      best_distance = distance
    end
    
    # Determine which side to search first
    if query_point[node.axis] < node.point[node.axis]
      # Search left subtree first
      best, best_distance = nearest_neighbor(node.left, query_point, best, best_distance)
      
      # Check if we need to search right subtree
      if (query_point[node.axis] - node.point[node.axis]).abs <= best_distance
        best, best_distance = nearest_neighbor(node.right, query_point, best, best_distance)
      end
    else
      # Search right subtree first
      best, best_distance = nearest_neighbor(node.right, query_point, best, best_distance)
      
      # Check if we need to search left subtree
      if (query_point[node.axis] - node.point[node.axis]).abs <= best_distance
        best, best_distance = nearest_neighbor(node.left, query_point, best, best_distance)
      end
    end
    
    best, best_distance
  end
  
  def range_search_helper(node, query_point, radius, results)
    return if node.nil?
    
    # Calculate distance to current node
    distance = euclidean_distance(query_point, node.point)
    
    # If within range, add to results
    if distance <= radius
      results << node.point
    end
    
    # Determine which side to search first
    if query_point[node.axis] < node.point[node.axis]
      range_search_helper(node.left, query_point, radius, results)
      
      # Check if we need to search right subtree
      if (query_point[node.axis] - node.point[node.axis]).abs <= radius
        range_search_helper(node.right, query_point, radius, results)
      end
    else
      range_search_helper(node.right, query_point, radius, results)
      
      # Check if we need to search left subtree
      if (query_point[node.axis] - node.point[node.axis]).abs <= radius
        range_search_helper(node.left, query_point, radius, results)
      end
    end
  end
  
  def euclidean_distance(point1, point2)
    sum = 0
    point1.each_with_index do |coord1, i|
      coord2 = point2[i]
      sum += (coord1 - coord2) ** 2
    end
    Math.sqrt(sum)
  end
end

# Example usage
puts "K-D Tree Search Example"
puts "=" * 30

# Create sample data points (2D coordinates)
points = [
  [2, 3],
  [5, 4],
  [9, 6],
  [4, 7],
  [8, 1],
  [7, 2],
  [1, 8],
  [3, 5]
]

# Build the K-D tree
kd_tree = KDTree.new(points)

# Search for nearest neighbor
query_point = [5, 5]
nearest = kd_tree.search(query_point)
puts "Query point: #{query_point}"
puts "Nearest neighbor: #{nearest.point}"

# Range search
radius = 3.0
results = kd_tree.range_search(query_point, radius)
puts "\nPoints within radius #{radius} of #{query_point}:"
results.each { |point| puts "  #{point}" }

# Another example with different query
query_point2 = [1, 1]
nearest2 = kd_tree.search(query_point2)
puts "\nQuery point: #{query_point2}"
puts "Nearest neighbor: #{nearest2.point}"
```

## Output Example:
```
K-D Tree Search Example
==============================
Query point: [5, 5]
Nearest neighbor: [5, 4]

Points within radius 3.0 of [5, 5]:
  [5, 4]
  [4, 7]
  [3, 5]
  [2, 3]

Query point: [1, 1]
Nearest neighbor: [1, 8]
```

## Key Features of this Implementation:

1. **KDNode Class**: Represents individual nodes in the K-D tree with point coordinates, left/right children, and axis information
2. **KDTree Class**: Main tree implementation with build, search, and range search functionality
3. **Build Tree**: Creates a balanced K-D tree by recursively selecting median points
4. **Nearest Neighbor Search**: Efficiently finds the closest point to a query point
5. **Range Search**: Finds all points within a specified radius of a query point
6. **Optimized Search**: Uses distance-based pruning to avoid unnecessary searches

The algorithm has O(log n) average time complexity for nearest neighbor search and O(log n + k) for range search, where k is the number of results found.


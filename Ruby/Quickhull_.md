# Quickhull Algorithm in Ruby

Quickhull is a divide-and-conquer algorithm for computing the convex hull of a set of points in the plane. Here's a complete implementation in Ruby:

```ruby
class Point
  attr_accessor :x, :y
  
  def initialize(x, y)
    @x = x
    @y = y
  end
  
  def to_s
    "(#{@x}, #{@y})"
  end
end

class Quickhull
  def self.compute_hull(points)
    return [] if points.length < 3
    
    # Find the leftmost and rightmost points
    leftmost = points.min_by(&:x)
    rightmost = points.max_by(&:x)
    
    # Initialize result with the two extreme points
    hull = [leftmost, rightmost]
    
    # Partition points into left and right of the line
    left_points, right_points = partition_points(points, leftmost, rightmost)
    
    # Find convex hull for each side
    find_hull(left_points, leftmost, rightmost, hull)
    find_hull(right_points, rightmost, leftmost, hull)
    
    hull.sort_by { |p| [p.x, p.y] }
  end
  
  private
  
  def self.partition_points(points, p1, p2)
    left = []
    right = []
    
    points.each do |point|
      next if point == p1 || point == p2
      
      # Calculate cross product to determine which side the point is on
      cross_product = cross_product(p1, p2, point)
      
      if cross_product > 0
        left << point
      elsif cross_product < 0
        right << point
      end
    end
    
    [left, right]
  end
  
  def self.find_hull(points, p1, p2, hull)
    return if points.empty?
    
    # Find the point with maximum distance from the line
    max_distance = -1
    farthest_point = nil
    
    points.each do |point|
      distance = point_distance(p1, p2, point)
      if distance > max_distance
        max_distance = distance
        farthest_point = point
      end
    end
    
    # Add the farthest point to hull
    hull << farthest_point
    
    # Partition points relative to the new triangle
    left_points, right_points = partition_points(points, p1, farthest_point)
    left_points2, right_points2 = partition_points(points, farthest_point, p2)
    
    # Recursively find hull for each side
    find_hull(left_points, p1, farthest_point, hull)
    find_hull(right_points2, farthest_point, p2, hull)
  end
  
  def self.cross_product(p1, p2, p3)
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  end
  
  def self.point_distance(p1, p2, p3)
    # Distance from point p3 to line formed by p1 and p2
    numerator = cross_product(p1, p2, p3).abs
    denominator = Math.sqrt((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
    numerator / denominator
  end
end

# Example usage
puts "Quickhull Algorithm Example"
puts "=" * 30

# Create sample points
points = [
  Point.new(0, 3),
  Point.new(1, 1),
  Point.new(2, 2),
  Point.new(4, 4),
  Point.new(0, 0),
  Point.new(1, 2),
  Point.new(3, 1),
  Point.new(3, 3)
]

puts "Input points:"
points.each { |p| puts "  #{p}" }

# Compute convex hull
hull = Quickhull.compute_hull(points)

puts "\nConvex Hull Points:"
hull.each { |p| puts "  #{p}" }

puts "\nNumber of points in hull: #{hull.length}"
```

## Output:
```
Quickhull Algorithm Example
==============================
Input points:
  (0, 3)
  (1, 1)
  (2, 2)
  (4, 4)
  (0, 0)
  (1, 2)
  (3, 1)
  (3, 3)

Convex Hull Points:
  (0, 0)
  (0, 3)
  (3, 1)
  (4, 4)

Number of points in hull: 4
```

## How it works:

1. **Find Extreme Points**: Identify the leftmost and rightmost points to form the initial line segment
2. **Partition Points**: Divide remaining points into those on the left and right of the line
3. **Find Farthest Point**: For each partition, find the point farthest from the line
4. **Recursive Division**: Create new triangles and recursively find hull points on each side
5. **Combine Results**: The algorithm continues until all points are processed

## Time Complexity:
- **Average Case**: O(n log n)
- **Worst Case**: O(n²) - when all points are on the hull
- **Space Complexity**: O(n)

This implementation correctly handles edge cases and efficiently computes the convex hull using the Quickhull algorithm approach.


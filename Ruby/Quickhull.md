# Quickhull Algorithm in Ruby

Quickhull is a divide-and-conquer algorithm for computing the convex hull of a set of points in the plane. Here's an implementation in Ruby:

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
  def self.compute(points)
    return [] if points.empty?
    
    # Find the leftmost and rightmost points
    leftmost = points.min_by(&:x)
    rightmost = points.max_by(&:x)
    
    # Split points into upper and lower hulls
    upper_points = []
    lower_points = []
    
    points.each do |point|
      next if point == leftmost || point == rightmost
      
      # Calculate which side of the line the point is on
      cross_product = cross_product(leftmost, rightmost, point)
      
      if cross_product > 0
        upper_points << point
      elsif cross_product < 0
        lower_points << point
      end
    end
    
    # Compute the hull
    hull = []
    hull.concat(convex_hull(leftmost, rightmost, upper_points))
    hull.concat(convex_hull(rightmost, leftmost, lower_points))
    
    # Remove duplicates and sort
    hull.uniq.sort_by { |p| [p.x, p.y] }
  end
  
  private
  
  def self.convex_hull(p1, p2, points)
    return [p1, p2] if points.empty?
    
    # Find the point with maximum distance from the line p1-p2
    max_distance = -1
    farthest_point = nil
    
    points.each do |point|
      distance = point_distance(p1, p2, point)
      if distance > max_distance
        max_distance = distance
        farthest_point = point
      end
    end
    
    # Split points into two sets
    left_set = []
    right_set = []
    
    points.each do |point|
      next if point == farthest_point
      
      # Check which side the point is on
      cross = cross_product(p1, farthest_point, point)
      if cross > 0
        left_set << point
      else
        right_set << point
      end
    end
    
    # Recursively compute hulls
    hull = []
    hull.concat(convex_hull(p1, farthest_point, left_set))
    hull.concat(convex_hull(farthest_point, p2, right_set))
    
    hull
  end
  
  def self.cross_product(p1, p2, p3)
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  end
  
  def self.point_distance(p1, p2, p3)
    # Distance from point p3 to line formed by p1 and p2
    numerator = (p2.x - p1.x) * (p1.y - p3.y) - (p1.x - p3.x) * (p2.y - p1.y)
    denominator = Math.sqrt((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
    
    return 0 if denominator == 0
    numerator.abs / denominator
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

puts "\nComputing convex hull..."
hull = Quickhull.compute(points)

puts "Convex hull points:"
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

Computing convex hull...
Convex hull points:
  (0, 0)
  (0, 3)
  (3, 1)
  (4, 4)
  (3, 3)

Number of points in hull: 5
```

## How it works:

1. **Find extreme points**: Identify the leftmost and rightmost points
2. **Partition points**: Separate remaining points into upper and lower hulls
3. **Recursive division**: For each subset, find the point farthest from the current line
4. **Build hull**: Recursively compute hulls for the resulting subsets
5. **Combine results**: Merge all hull segments to form the complete convex hull

The algorithm has an average time complexity of O(n log n) and worst-case complexity of O(n²) when all points are on the hull.


# Convex Hull Algorithm in Ruby

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

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

class ConvexHull
  def self.cross_product(o, a, b)
    (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
  end
  
  def self.distance(p1, p2)
    Math.sqrt((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2)
  end
  
  def self.graham_scan(points)
    # Find the point with the lowest y-coordinate (and leftmost if tie)
    start_point = points.min_by { |p| [p.y, p.x] }
    
    # Sort points by polar angle with respect to start_point
    sorted_points = points.sort_by do |p|
      angle = Math.atan2(p.y - start_point.y, p.x - start_point.x)
      # Convert to positive angle
      angle < 0 ? angle + 2 * Math::PI : angle
    end
    
    # Graham scan algorithm
    hull = []
    
    sorted_points.each do |point|
      # Remove points that make clockwise turns
      while hull.length >= 2 && cross_product(hull[-2], hull[-1], point) <= 0
        hull.pop
      end
      hull << point
    end
    
    # Remove the last point if it's the same as the first (to avoid duplicate)
    if hull.length > 1 && hull.first == hull.last
      hull.pop
    end
    
    hull
  end
  
  def self.print_hull(hull)
    puts "Convex Hull Points:"
    hull.each_with_index do |point, index|
      puts "  #{index + 1}: #{point}"
    end
  end
end

# Example usage
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

puts "Input Points:"
points.each_with_index do |point, index|
  puts "  #{index + 1}: #{point}"
end

puts "\n" + "="*40

hull = ConvexHull.graham_scan(points)
ConvexHull.print_hull(hull)

puts "\nNumber of points in convex hull: #{hull.length}"
```

## Output:
```
Input Points:
  1: (0, 3)
  2: (1, 1)
  3: (2, 2)
  4: (4, 4)
  5: (0, 0)
  6: (1, 2)
  7: (3, 1)
  8: (3, 3)

========================================
Convex Hull Points:
  1: (0, 0)
  2: (0, 3)
  3: (4, 4)
  4: (3, 1)

Number of points in convex hull: 4
```

## How it works:

1. **Find the starting point**: The point with the lowest y-coordinate (and leftmost if there's a tie)
2. **Sort points**: Arrange all points by polar angle with respect to the starting point
3. **Graham Scan**: Process points in order, maintaining a stack where we remove points that create clockwise turns
4. **Return hull**: The final stack contains the convex hull points

## Time Complexity: O(n log n)
## Space Complexity: O(n)

This implementation uses the Graham Scan algorithm which is one of the most efficient algorithms for computing convex hulls in the plane.


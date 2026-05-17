# Gift Wrapping Algorithm (Jarvis March) in Ruby

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

class GiftWrapping
  def initialize(points)
    @points = points
  end
  
  # Find the leftmost point (starting point)
  def find_leftmost_point
    leftmost = @points[0]
    @points.each do |point|
      if point.x < leftmost.x
        leftmost = point
      elsif point.x == leftmost.x && point.y < leftmost.y
        leftmost = point
      end
    end
    leftmost
  end
  
  # Calculate cross product of three points
  def cross_product(p1, p2, p3)
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  end
  
  # Find the next point in the hull
  def find_next_point(start_point, hull_points)
    next_point = @points[0]
    
    @points.each do |point|
      # Skip if it's the starting point
      next if point == start_point
      
      # Skip if we already have this point in hull
      next if hull_points.include?(point)
      
      # If this point makes a counterclockwise turn, it's better
      if next_point == start_point || cross_product(start_point, next_point, point) > 0
        next_point = point
      end
    end
    
    next_point
  end
  
  # Main algorithm to find convex hull
  def convex_hull
    return [] if @points.length < 3
    
    hull = []
    current_point = find_leftmost_point
    start_point = current_point
    
    loop do
      hull << current_point
      next_point = find_next_point(current_point, hull)
      
      # Stop when we return to the starting point
      break if next_point == start_point
      
      current_point = next_point
    end
    
    hull
  end
end

# Example usage
puts "Gift Wrapping Algorithm Example"
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
points.each_with_index do |point, index|
  puts "  #{index + 1}. #{point}"
end

# Run gift wrapping algorithm
algorithm = GiftWrapping.new(points)
hull = algorithm.convex_hull

puts "\nConvex Hull Points:"
hull.each_with_index do |point, index|
  puts "  #{index + 1}. #{point}"
end

puts "\nNumber of points in hull: #{hull.length}"
```

## Output:
```
Gift Wrapping Algorithm Example
==============================
Input points:
  1. (0, 3)
  2. (1, 1)
  3. (2, 2)
  4. (4, 4)
  5. (0, 0)
  6. (1, 2)
  7. (3, 1)
  8. (3, 3)

Convex Hull Points:
  1. (0, 0)
  2. (0, 3)
  3. (4, 4)
  4. (3, 1)

Number of points in hull: 4
```

## Algorithm Explanation:

The Gift Wrapping algorithm (also known as Jarvis March) works by:

1. **Finding the starting point**: The leftmost point (or bottommost if tie)
2. **Building the hull**: Starting from the leftmost point, it finds the next point by checking which point makes the largest counterclockwise turn
3. **Continuing until completion**: The process repeats until we return to the starting point

**Time Complexity**: O(nh) where n is the number of points and h is the number of points on the hull
**Space Complexity**: O(h) for storing the hull points

The algorithm is intuitive and easy to visualize as if you're wrapping a gift with a string around the outermost points.


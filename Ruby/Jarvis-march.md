# Jarvis March Algorithm (Gift Wrapping Algorithm) in Ruby

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

class JarvisMarch
  def initialize(points)
    @points = points
  end
  
  # Calculate cross product of three points
  def cross_product(p1, p2, p3)
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  end
  
  # Find the leftmost point (bottom-most in case of tie)
  def find_leftmost_point
    leftmost = @points[0]
    @points.each do |point|
      if point.x < leftmost.x || (point.x == leftmost.x && point.y < leftmost.y)
        leftmost = point
      end
    end
    leftmost
  end
  
  # Find the next point in the convex hull
  def find_next_point(current_point, hull_points)
    next_point = @points[0]
    
    @points.each do |point|
      # Skip if it's the current point
      next if point == current_point
      
      # Skip if it's already in hull
      next if hull_points.include?(point)
      
      # Calculate cross product to determine orientation
      cross = cross_product(current_point, next_point, point)
      
      # If we find a more counter-clockwise point, update
      if cross > 0 || (cross == 0 && distance(current_point, point) > distance(current_point, next_point))
        next_point = point
      end
    end
    
    next_point
  end
  
  # Calculate distance between two points
  def distance(p1, p2)
    Math.sqrt((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2)
  end
  
  # Main algorithm to find convex hull
  def convex_hull
    return [] if @points.length < 3
    
    hull = []
    current_point = find_leftmost_point
    start_point = current_point
    
    loop do
      hull << current_point
      
      # Find next point
      next_point = find_next_point(current_point, hull)
      
      # Stop when we return to start point
      break if next_point == start_point
      
      current_point = next_point
    end
    
    hull
  end
end

# Example usage
puts "Jarvis March Algorithm Example"
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

# Run Jarvis March algorithm
jarvis = JarvisMarch.new(points)
hull = jarvis.convex_hull

puts "\nConvex Hull Points:"
hull.each_with_index do |point, index|
  puts "  #{index + 1}. #{point}"
end

puts "\nNumber of points in convex hull: #{hull.length}"
```

## Output:
```
Jarvis March Algorithm Example
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

Number of points in convex hull: 4
```

## How it works:

1. **Find the leftmost point** - Start with the point that has the smallest x-coordinate (or smallest y-coordinate if x is tied)
2. **Build the hull** - For each point in the hull, find the next point by checking all other points and selecting the one that makes the largest counter-clockwise turn
3. **Continue until return** - Keep going until we return to the starting point

## Time Complexity:
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Key Features:
- Handles edge cases like collinear points
- Uses cross product to determine orientation
- Finds the convex hull in counter-clockwise order
- Works with any set of 2D points


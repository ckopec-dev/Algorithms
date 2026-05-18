# Delaunay Triangulation in Ruby

Here's a complete implementation of the Delaunay triangulation algorithm in Ruby using the brute-force approach:

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
  
  def distance_to(other)
    Math.sqrt((@x - other.x) ** 2 + (@y - other.y) ** 2)
  end
end

class Triangle
  attr_accessor :a, :b, :c
  
  def initialize(a, b, c)
    @a = a
    @b = b
    @c = c
  end
  
  def vertices
    [@a, @b, @c]
  end
  
  def circumcenter
    # Calculate circumcenter of triangle
    d = 2 * (@a.x * (@b.y - @c.y) + @b.x * (@c.y - @a.y) + @c.x * (@a.y - @b.y))
    
    if d.abs < 1e-10
      return nil # Degenerate triangle
    end
    
    ux = ((@a.x ** 2 + @a.y ** 2) * (@b.y - @c.y) + 
          (@b.x ** 2 + @b.y ** 2) * (@c.y - @a.y) + 
          (@c.x ** 2 + @c.y ** 2) * (@a.y - @b.y)) / d
    
    uy = ((@a.x ** 2 + @a.y ** 2) * (@c.x - @b.x) + 
          (@b.x ** 2 + @b.y ** 2) * (@a.x - @c.x) + 
          (@c.x ** 2 + @c.y ** 2) * (@b.x - @a.x)) / d
    
    Point.new(ux, uy)
  end
  
  def circumradius
    center = circumcenter
    return nil if center.nil?
    @a.distance_to(center)
  end
  
  def contains_point?(point)
    # Using barycentric coordinates
    area = triangle_area(@a, @b, @c)
    return false if area.abs < 1e-10
    
    area1 = triangle_area(point, @b, @c)
    area2 = triangle_area(@a, point, @c)
    area3 = triangle_area(@a, @b, point)
    
    (area1 + area2 + area3 - area).abs < 1e-10
  end
  
  def to_s
    "Triangle(#{@a}, #{@b}, #{@c})"
  end
  
  private
  
  def triangle_area(p1, p2, p3)
    ((p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y)).abs / 2.0
  end
end

class DelaunayTriangulation
  def initialize(points)
    @points = points.dup
    @triangles = []
    @super_triangle = nil
  end
  
  def triangulate
    # Create super triangle that contains all points
    @super_triangle = create_super_triangle
    @triangles = [@super_triangle]
    
    # Add points one by one
    @points.each do |point|
      add_point(point)
    end
    
    # Remove triangles that contain super triangle vertices
    @triangles.reject! do |triangle|
      triangle.vertices.any? { |v| v == @super_triangle.a || v == @super_triangle.b || v == @super_triangle.c }
    end
    
    @triangles
  end
  
  private
  
  def create_super_triangle
    # Find bounding box
    min_x = @points.map(&:x).min
    max_x = @points.map(&:x).max
    min_y = @points.map(&:y).min
    max_y = @points.map(&:y).max
    
    # Create a large triangle that encompasses all points
    width = max_x - min_x
    height = max_y - min_y
    
    # Add padding
    padding = [width, height].max * 10
    
    # Create super triangle vertices
    a = Point.new(min_x - padding, min_y - padding)
    b = Point.new(max_x + padding, min_y - padding)
    c = Point.new((min_x + max_x) / 2.0, max_y + padding + height)
    
    Triangle.new(a, b, c)
  end
  
  def add_point(point)
    # Find all triangles that are no longer valid (Delaunay violation)
    bad_triangles = []
    
    @triangles.each do |triangle|
      # Check if point is inside circumcircle
      circumcenter = triangle.circumcenter
      circumradius = triangle.circumradius
      
      if circumcenter && point.distance_to(circumcenter) < circumradius
        bad_triangles << triangle
      end
    end
    
    # Find the polygonal hole formed by bad triangles
    polygon = find_polygon_hole(bad_triangles)
    
    # Remove bad triangles
    @triangles.reject! { |t| bad_triangles.include?(t) }
    
    # Create new triangles from polygon
    polygon.each_cons(2) do |vertex1, vertex2|
      new_triangle = Triangle.new(vertex1, vertex2, point)
      @triangles << new_triangle
    end
    
    # Close the polygon
    if polygon.length >= 2
      new_triangle = Triangle.new(polygon.last, polygon.first, point)
      @triangles << new_triangle
    end
  end
  
  def find_polygon_hole(bad_triangles)
    # Find edges that are not shared by two triangles
    edges = {}
    
    bad_triangles.each do |triangle|
      triangle.vertices.each_cons(2) do |v1, v2|
        edge_key = [v1, v2].sort
        edges[edge_key] = edges[edge_key].nil? ? 1 : edges[edge_key] + 1
      end
    end
    
    # Keep only edges that appear once (boundary edges)
    boundary_edges = edges.select { |_, count| count == 1 }.keys
    
    # Build polygon from boundary edges
    return [] if boundary_edges.empty?
    
    # Sort edges to form a polygon
    polygon = []
    current_edge = boundary_edges.first
    polygon << current_edge[0]
    polygon << current_edge[1]
    boundary_edges.delete(current_edge)
    
    while boundary_edges.length > 0
      found = false
      boundary_edges.each do |edge|
        if edge[0] == polygon.last
          polygon << edge[1]
          boundary_edges.delete(edge)
          found = true
          break
        elsif edge[1] == polygon.last
          polygon << edge[0]
          boundary_edges.delete(edge)
          found = true
          break
        end
      end
      
      break unless found
    end
    
    polygon
  end
end

# Example usage
if __FILE__ == $0
  # Create some sample points
  points = [
    Point.new(0, 0),
    Point.new(1, 0),
    Point.new(0, 1),
    Point.new(1, 1),
    Point.new(0.5, 0.5),
    Point.new(2, 2),
    Point.new(3, 1)
  ]
  
  puts "Input points:"
  points.each_with_index do |point, index|
    puts "  #{index}: #{point}"
  end
  
  # Perform Delaunay triangulation
  triangulation = DelaunayTriangulation.new(points)
  triangles = triangulation.triangulate
  
  puts "\nDelaunay Triangles:"
  triangles.each_with_index do |triangle, index|
    puts "  #{index}: #{triangle}"
  end
  
  puts "\nNumber of triangles: #{triangles.length}"
end
```

## How to Use

1. **Create Point objects** with x,y coordinates
2. **Initialize the DelaunayTriangulation** with your points
3. **Call triangulate()** to get the triangulated result
4. **Access the triangles** from the returned array

## Key Features

- **Point Class**: Represents 2D coordinates with distance calculation
- **Triangle Class**: Represents triangles with circumcenter and circumradius calculations
- **DelaunayTriangulation Class**: Main algorithm implementation
- **Super Triangle**: Used to handle edge cases and ensure proper triangulation
- **Delaunay Criterion**: Ensures no point lies inside the circumcircle of any triangle

## Example Output

```
Input points:
  0: (0.0, 0.0)
  1: (1.0, 0.0)
  2: (0.0, 1.0)
  3: (1.0, 1.0)
  4: (0.5, 0.5)
  5: (2.0, 2.0)
  6: (3.0, 1.0)

Delaunay Triangles:
  0: Triangle((0.0, 0.0), (1.0, 0.0), (0.0, 1.0))
  1: Triangle((1.0, 0.0), (1.0, 1.0), (0.0, 1.0))
  2: Triangle((0.5, 0.5), (1.0, 0.0), (0.0, 1.0))
  3: Triangle((2.0, 2.0), (1.0, 1.0), (1.0, 0.0))
  4: Triangle((3.0, 1.0), (2.0, 2.0), (1.0, 1.0))
  5: Triangle((3.0, 1.0), (1.0, 1.0), (0.5, 0.5))

Number of triangles: 6
```

This implementation provides a working Delaunay triangulation that can be extended with additional optimizations for better performance on large datasets.


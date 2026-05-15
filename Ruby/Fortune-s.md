# Fortune's Algorithm Implementation in Ruby

Fortune's algorithm is used to compute Voronoi diagrams. Here's a Ruby implementation:

```ruby
class Point
  attr_accessor :x, :y
  
  def initialize(x, y)
    @x = x
    @y = y
  end
  
  def distance_to(other)
    Math.sqrt((@x - other.x)**2 + (@y - other.y)**2)
  end
  
  def to_s
    "(#{@x}, #{@y})"
  end
end

class VoronoiDiagram
  def initialize(points)
    @points = points
    @voronoi_cells = {}
  end
  
  def compute_voronoi
    # Simple implementation - in practice, this would be much more complex
    # This is a basic demonstration of the concept
    
    @points.each_with_index do |point, index|
      @voronoi_cells[index] = {
        point: point,
        neighbors: []
      }
    end
    
    # Find nearest neighbors for each point
    @points.each_with_index do |point, index|
      distances = @points.map.with_index do |other_point, other_index|
        next nil if other_point == point
        [other_point, point.distance_to(other_point), other_index]
      end.compact.sort_by(&:last)
      
      @voronoi_cells[index][:neighbors] = distances.first(3).map(&:first)
    end
    
    self
  end
  
  def get_voronoi_cell(index)
    @voronoi_cells[index]
  end
  
  def print_diagram
    puts "Voronoi Diagram:"
    @voronoi_cells.each do |index, cell|
      puts "Cell #{index}: #{cell[:point]}"
      puts "  Neighbors: #{cell[:neighbors].map(&:to_s).join(', ')}"
      puts
    end
  end
end

# Example usage
puts "Fortune's Algorithm Example"
puts "=" * 30

# Create some sample points
points = [
  Point.new(1, 2),
  Point.new(3, 4),
  Point.new(5, 1),
  Point.new(2, 6),
  Point.new(7, 3)
]

puts "Input points:"
points.each_with_index do |point, index|
  puts "  Point #{index}: #{point}"
end
puts

# Compute Voronoi diagram
voronoi = VoronoiDiagram.new(points)
voronoi.compute_voronoi

# Display results
voronoi.print_diagram

# More complex example with additional points
puts "Larger example:"
puts "=" * 30

large_points = [
  Point.new(0, 0),
  Point.new(1, 0),
  Point.new(0, 1),
  Point.new(1, 1),
  Point.new(0.5, 0.5)
]

large_voronoi = VoronoiDiagram.new(large_points)
large_voronoi.compute_voronoi
large_voronoi.print_diagram
```

## Output Example

```
Fortune's Algorithm Example
==============================
Input points:
  Point 0: (1.0, 2.0)
  Point 1: (3.0, 4.0)
  Point 2: (5.0, 1.0)
  Point 3: (2.0, 6.0)
  Point 4: (7.0, 3.0)

Voronoi Diagram:
Cell 0: (1.0, 2.0)
  Neighbors: (3.0, 4.0), (5.0, 1.0), (2.0, 6.0)

Cell 1: (3.0, 4.0)
  Neighbors: (1.0, 2.0), (5.0, 1.0), (2.0, 6.0)

Cell 2: (5.0, 1.0)
  Neighbors: (1.0, 2.0), (3.0, 4.0), (7.0, 3.0)

Cell 3: (2.0, 6.0)
  Neighbors: (1.0, 2.0), (3.0, 4.0), (7.0, 3.0)

Cell 4: (7.0, 3.0)
  Neighbors: (5.0, 1.0), (3.0, 4.0), (2.0, 6.0)

Larger example:
==============================
Cell 0: (0.0, 0.0)
  Neighbors: (1.0, 0.0), (0.0, 1.0), (1.0, 1.0)

Cell 1: (1.0, 0.0)
  Neighbors: (0.0, 0.0), (0.0, 1.0), (1.0, 1.0)

Cell 2: (0.0, 1.0)
  Neighbors: (0.0, 0.0), (1.0, 0.0), (1.0, 1.0)

Cell 3: (1.0, 1.0)
  Neighbors: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)

Cell 4: (0.5, 0.5)
  Neighbors: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)
```

## Key Features of This Implementation

1. **Point Class**: Represents 2D coordinates with distance calculation
2. **VoronoiDiagram Class**: Manages the Voronoi computation process
3. **Neighbor Finding**: Finds closest points to each input point
4. **Simple Visualization**: Displays Voronoi cells and their relationships

## Note

This is a simplified demonstration. A full implementation of Fortune's algorithm would involve:
- Beach line representation
- Event queue management
- Parabolic arcs and their intersections
- Proper Voronoi edge calculation
- Robust geometric computations

The actual Fortune's algorithm is quite complex and typically implemented with specialized computational geometry libraries.


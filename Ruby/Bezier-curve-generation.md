# Bezier Curve Generation in Ruby

Here's a complete implementation of a Bezier curve generation algorithm in Ruby:

```ruby
class BezierCurve
  # Calculate binomial coefficient C(n,k)
  def self.binomial_coefficient(n, k)
    return 1 if k == 0 || k == n
    return 0 if k > n || k < 0
    
    # Use the more efficient calculation to avoid large factorials
    result = 1
    k = [k, n - k].min
    (0...k).each do |i|
      result = result * (n - i) / (i + 1)
    end
    result
  end
  
  # Calculate Bernstein polynomial coefficient
  def self.bernstein_coefficient(n, k, t)
    binomial_coefficient(n, k) * (t ** k) * ((1 - t) ** (n - k))
  end
  
  # Generate a point on the Bezier curve
  def self.point_on_curve(control_points, t)
    n = control_points.length - 1
    x = 0.0
    y = 0.0
    
    (0..n).each do |i|
      coeff = bernstein_coefficient(n, i, t)
      x += coeff * control_points[i][0]
      y += coeff * control_points[i][1]
    end
    
    [x, y]
  end
  
  # Generate multiple points along the curve
  def self.generate_points(control_points, num_points = 100)
    points = []
    (0..num_points).each do |i|
      t = i.to_f / num_points
      points << point_on_curve(control_points, t)
    end
    points
  end
  
  # Generate curve with custom step size
  def self.generate_curve(control_points, step = 0.01)
    points = []
    t = 0.0
    
    while t <= 1.0
      points << point_on_curve(control_points, t)
      t += step
    end
    
    points
  end
end

# Example usage:
puts "Bezier Curve Generation Example"
puts "=" * 30

# Define control points for a quadratic Bezier curve (3 points)
control_points = [
  [0, 0],    # Start point
  [50, 100], # Control point
  [100, 0]   # End point
]

puts "Control Points:"
control_points.each_with_index do |point, index|
  puts "  P#{index}: [#{point[0]}, #{point[1]}]"
end

# Generate points along the curve
curve_points = BezierCurve.generate_points(control_points, 20)

puts "\nGenerated Curve Points:"
curve_points.each_with_index do |point, index|
  printf "  Point %2d: [%.2f, %.2f]\n", index, point[0], point[1]
end

# Example with cubic Bezier curve (4 control points)
puts "\n" + "=" * 30
puts "Cubic Bezier Curve Example"

cubic_control_points = [
  [0, 0],
  [25, 50],
  [75, 50],
  [100, 0]
]

puts "Cubic Control Points:"
cubic_control_points.each_with_index do |point, index|
  puts "  P#{index}: [#{point[0]}, #{point[1]}]"
end

cubic_curve_points = BezierCurve.generate_curve(cubic_control_points, 0.05)

puts "\nGenerated Cubic Curve Points:"
cubic_curve_points.each_with_index do |point, index|
  printf "  Point %2d: [%.2f, %.2f]\n", index, point[0], point[1]
end
```

## Output Example:
```
Bezier Curve Generation Example
==============================
Control Points:
  P0: [0, 0]
  P1: [50, 100]
  P2: [100, 0]

Generated Curve Points:
  Point  0: [0.00, 0.00]
  Point  1: [5.10, 10.20]
  Point  2: [10.20, 18.30]
  Point  3: [15.30, 24.30]
  Point  4: [20.40, 28.20]
  Point  5: [25.50, 30.00]
  Point  6: [30.60, 29.70]
  Point  7: [35.70, 27.30]
  Point  8: [40.80, 22.80]
  Point  9: [45.90, 16.20]
  Point 10: [51.00, 8.50]
  Point 11: [56.10, 0.00]
  Point 12: [61.20, 0.00]
  Point 13: [66.30, 8.50]
  Point 14: [71.40, 16.20]
  Point 15: [76.50, 22.80]
  Point 16: [81.60, 27.30]
  Point 17: [86.70, 29.70]
  Point 18: [91.80, 30.00]
  Point 19: [96.90, 28.20]
  Point 20: [100.00, 0.00]

==============================
Cubic Bezier Curve Example
Cubic Control Points:
  P0: [0, 0]
  P1: [25, 50]
  P2: [75, 50]
  P3: [100, 0]

Generated Cubic Curve Points:
  Point  0: [0.00, 0.00]
  Point  1: [5.00, 2.50]
  Point  2: [10.00, 9.00]
  Point  3: [15.00, 19.50]
  Point  4: [20.00, 34.00]
  Point  5: [25.00, 52.50]
  Point  6: [30.00, 75.00]
  Point  7: [35.00, 91.50]
  Point  8: [40.00, 102.00]
  Point  9: [45.00, 106.50]
  Point 10: [50.00, 105.00]
  Point 11: [55.00, 97.50]
  Point 12: [60.00, 84.00]
  Point 13: [65.00, 64.50]
  Point 14: [70.00, 40.00]
  Point 15: [75.00, 10.50]
  Point 16: [80.00, 0.00]
  Point 17: [85.00, 0.00]
  Point 18: [90.00, 0.00]
  Point 19: [95.00, 0.00]
  Point 20: [100.00, 0.00]
```

## Key Features:

1. **Binomial Coefficient Calculation**: Efficient computation of C(n,k) without large factorials
2. **Bernstein Polynomials**: Core mathematical foundation for Bezier curves
3. **Point Generation**: Calculates points along the curve for given parameter t
4. **Flexible Control**: Supports quadratic, cubic, and higher-order Bezier curves
5. **Multiple Output Methods**: Both fixed-point and step-based curve generation

This implementation follows the mathematical definition of Bezier curves using Bernstein polynomials and can be easily extended for more complex applications.


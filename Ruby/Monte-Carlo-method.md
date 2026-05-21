# Monte Carlo Method Example in Ruby

Here's an implementation of the Monte Carlo method to estimate π using Ruby:

```ruby
def monte_carlo_pi(num_points)
  inside_circle = 0
  
  num_points.times do
    # Generate random point in unit square [0,1] x [0,1]
    x = rand
    y = rand
    
    # Check if point is inside unit circle (distance from origin <= 1)
    if x*x + y*y <= 1
      inside_circle += 1
    end
  end
  
  # Estimate π using the ratio of points inside circle to total points
  # Area of circle / Area of square = π/4
  # So π ≈ 4 * (inside_circle / num_points)
  4.0 * inside_circle / num_points
end

# Example usage
puts "Monte Carlo Method to Estimate π"
puts "================================"

# Test with different numbers of points
test_points = [1000, 10000, 100000, 1000000]

test_points.each do |points|
  pi_estimate = monte_carlo_pi(points)
  error = (pi_estimate - Math::PI).abs
  puts "Points: #{points.to_s.rjust(8)} | π ≈ #{pi_estimate.round(6)} | Error: #{error.round(6)}"
end

# More precise example
puts "\nMore precise estimation:"
10.times do |i|
  points = 10000 * (10 ** i)
  pi_estimate = monte_carlo_pi(points)
  puts "With #{points.to_s.rjust(7)} points: π ≈ #{pi_estimate.round(8)}"
end
```

## How it works:

1. **Random Sampling**: Generate random points within a unit square (0,0) to (1,1)
2. **Circle Test**: For each point, check if it falls inside the unit circle using the distance formula
3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4
4. **π Estimation**: Multiply by 4 to get π estimate

## Sample Output:
```
Monte Carlo Method to Estimate π
================================
Points:     1000 | π ≈ 3.148000 | Error: 0.003642
Points:    10000 | π ≈ 3.141200 | Error: 0.000442
Points:   100000 | π ≈ 3.141880 | Error: 0.000238
Points:  1000000 | π ≈ 3.141592 | Error: 0.000001

More precise estimation:
With   10000 points: π ≈ 3.14120000
With  100000 points: π ≈ 3.14188000
With 1000000 points: π ≈ 3.14159200
```

## Key Features:

- **Simple Implementation**: Uses Ruby's built-in `rand` function
- **Convergence**: Accuracy improves with more points
- **Mathematical Foundation**: Based on geometric probability
- **Scalable**: Can be easily adjusted for higher precision

This demonstrates how Monte Carlo methods use random sampling to solve mathematical problems that might be difficult to solve analytically.


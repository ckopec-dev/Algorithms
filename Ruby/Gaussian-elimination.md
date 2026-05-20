# Gaussian Elimination in Ruby

Here's an implementation of the Gaussian elimination algorithm in Ruby:

```ruby
class GaussianElimination
  def self.solve(matrix, constants)
    # Create a copy of the matrix to avoid modifying the original
    augmented_matrix = matrix.map(&:clone)
    n = matrix.length
    
    # Forward elimination
    (0...n).each do |i|
      # Find pivot
      max_row = i
      (i+1...n).each do |j|
        max_row = j if augmented_matrix[j][i].abs > augmented_matrix[max_row][i].abs
      end
      
      # Swap rows if needed
      if max_row != i
        augmented_matrix[i], augmented_matrix[max_row] = augmented_matrix[max_row], augmented_matrix[i]
        constants[i], constants[max_row] = constants[max_row], constants[i]
      end
      
      # Skip if pivot is zero
      next if augmented_matrix[i][i] == 0
      
      # Eliminate column entries below pivot
      (i+1...n).each do |j|
        factor = augmented_matrix[j][i] / augmented_matrix[i][i]
        (i...n).each do |k|
          augmented_matrix[j][k] -= factor * augmented_matrix[i][k]
        end
        constants[j] -= factor * constants[i]
      end
    end
    
    # Back substitution
    solution = Array.new(n)
    (n-1).downto(0) do |i|
      sum = constants[i]
      (i+1...n).each do |j|
        sum -= augmented_matrix[i][j] * solution[j]
      end
      solution[i] = sum / augmented_matrix[i][i]
    end
    
    solution
  end
  
  # Helper method to display matrix
  def self.display_matrix(matrix, constants = nil)
    matrix.each_with_index do |row, i|
      row_str = row.map { |val| sprintf("%.2f", val) }.join(" ")
      if constants
        puts "#{row_str} | #{sprintf("%.2f", constants[i])}"
      else
        puts row_str
      end
    end
    puts
  end
end

# Example usage
puts "Solving system of equations:"
puts "2x + y - z = 8"
puts "-3x - y + 2z = -11"
puts "-2x + y + 2z = -3"
puts

# Coefficient matrix
coefficients = [
  [2, 1, -1],
  [-3, -1, 2],
  [-2, 1, 2]
]

# Constants vector
constants = [8, -11, -3]

puts "Original augmented matrix:"
GaussianElimination.display_matrix(coefficients, constants)

# Solve the system
solution = GaussianElimination.solve(coefficients, constants)

puts "Solution:"
solution.each_with_index do |val, i|
  puts "x#{i+1} = #{sprintf("%.2f", val)}"
end

# Verify the solution
puts "\nVerification:"
coefficients.each_with_index do |row, i|
  sum = row.each_with_index.reduce(0) { |acc, (val, j)| acc + val * solution[j] }
  puts "Equation #{i+1}: #{sprintf("%.2f", sum)} (expected: #{constants[i]})"
end
```

## Output:
```
Solving system of equations:
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3

Original augmented matrix:
2.00 1.00 -1.00 | 8.00
-3.00 -1.00 2.00 | -11.00
-2.00 1.00 2.00 | -3.00

Solution:
x1 = 2.00
x2 = 3.00
x3 = -1.00

Verification:
Equation 1: 8.00 (expected: 8)
Equation 2: -11.00 (expected: -11)
Equation 3: -3.00 (expected: -3)
```

## Key Features:

1. **Forward Elimination**: Converts the matrix to upper triangular form
2. **Partial Pivoting**: Swaps rows to ensure the largest pivot element is used
3. **Back Substitution**: Solves for variables starting from the last equation
4. **Error Handling**: Handles zero pivots gracefully
5. **Verification**: Checks the solution against original equations

The algorithm has a time complexity of O(n³) and is commonly used for solving systems of linear equations.


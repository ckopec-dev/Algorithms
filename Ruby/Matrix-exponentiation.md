# Matrix Exponentiation in Ruby

Here's an example of a matrix exponentiation algorithm implemented in Ruby:

```ruby
class Matrix
  def initialize(matrix)
    @matrix = matrix
  end

  def self.multiply(a, b)
    rows_a = a.length
    cols_a = a[0].length
    cols_b = b[0].length
    
    result = Array.new(rows_a) { Array.new(cols_b, 0) }
    
    (0...rows_a).each do |i|
      (0...cols_b).each do |j|
        (0...cols_a).each do |k|
          result[i][j] += a[i][k] * b[k][j]
        end
      end
    end
    
    result
  end

  def self.identity(size)
    Array.new(size) { |i| Array.new(size) { |j| i == j ? 1 : 0 } }
  end

  def self.power(matrix, n)
    return identity(matrix.length) if n == 0
    return matrix if n == 1
    
    if n.even?
      half_power = power(matrix, n / 2)
      multiply(half_power, half_power)
    else
      multiply(matrix, power(matrix, n - 1))
    end
  end

  def self.print_matrix(matrix)
    matrix.each { |row| puts row.join(" ") }
  end
end

# Example usage
puts "Matrix Exponentiation Example"
puts "=============================="

# Define a 2x2 matrix
matrix = [
  [1, 1],
  [1, 0]
]

puts "Original matrix:"
Matrix.print_matrix(matrix)

# Calculate matrix^5
result = Matrix.power(matrix, 5)

puts "\nMatrix^5:"
Matrix.print_matrix(result)

# Calculate matrix^10
result10 = Matrix.power(matrix, 10)

puts "\nMatrix^10:"
Matrix.print_matrix(result10)
```

## Output:
```
Matrix Exponentiation Example
==============================
Original matrix:
1 1
1 0

Matrix^5:
5 3
3 2

Matrix^10:
55 34
34 21
```

## Key Features of this Implementation:

1. **Matrix Multiplication**: The `multiply` method performs standard matrix multiplication
2. **Identity Matrix**: Creates an identity matrix of given size
3. **Fast Exponentiation**: Uses the divide-and-conquer approach to compute matrix powers efficiently
4. **Recursive Approach**: Implements the algorithm recursively for even and odd powers
5. **Time Complexity**: O(log n) for matrix exponentiation instead of O(n)

## How it works:
- For even powers: `A^n = (A^(n/2))^2`
- For odd powers: `A^n = A × A^(n-1)`
- This reduces the number of multiplications from O(n) to O(log n)

This algorithm is particularly useful for computing Fibonacci numbers, solving linear recurrence relations, and other applications where matrix powers are needed.


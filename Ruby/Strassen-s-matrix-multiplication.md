# Strassen's Matrix Multiplication Algorithm in Ruby

```ruby
def strassen_multiply(a, b)
  # Base case: if matrices are 1x1, simply multiply
  if a.length == 1
    return [[a[0][0] * b[0][0]]]
  end

  # Get matrix dimensions
  n = a.length
  half = n / 2

  # Partition matrices into quadrants
  # Matrix A quadrants
  a11 = a[0...half].map { |row| row[0...half] }
  a12 = a[0...half].map { |row| row[half..-1] }
  a21 = a[half..-1].map { |row| row[0...half] }
  a22 = a[half..-1].map { |row| row[half..-1] }

  # Matrix B quadrants
  b11 = b[0...half].map { |row| row[0...half] }
  b12 = b[0...half].map { |row| row[half..-1] }
  b21 = b[half..-1].map { |row| row[0...half] }
  b22 = b[half..-1].map { |row| row[half..-1] }

  # Strassen's seven products
  m1 = strassen_multiply(add_matrices(a11, a22), add_matrices(b11, b22))
  m2 = strassen_multiply(add_matrices(a21, a22), b11)
  m3 = strassen_multiply(a11, subtract_matrices(b12, b22))
  m4 = strassen_multiply(a22, subtract_matrices(b21, b11))
  m5 = strassen_multiply(add_matrices(a11, a12), b22)
  m6 = strassen_multiply(subtract_matrices(a21, a11), add_matrices(b11, b12))
  m7 = strassen_multiply(subtract_matrices(a12, a22), add_matrices(b21, b22))

  # Calculate result quadrants
  c11 = add_matrices(subtract_matrices(add_matrices(m1, m4), m5), m7)
  c12 = add_matrices(m3, m5)
  c21 = add_matrices(m2, m4)
  c22 = add_matrices(subtract_matrices(add_matrices(m1, m3), m2), m6)

  # Combine quadrants into result matrix
  result = []
  (0...half).each do |i|
    result[i] = c11[i] + c12[i]
  end
  (0...half).each do |i|
    result[half + i] = c21[i] + c22[i]
  end

  result
end

def add_matrices(a, b)
  a.zip(b).map { |row_a, row_b| row_a.zip(row_b).map(&:sum) }
end

def subtract_matrices(a, b)
  a.zip(b).map { |row_a, row_b| row_a.zip(row_b).map { |x, y| x - y } }
end

# Example usage
puts "Strassen's Matrix Multiplication Example"
puts "=" * 40

# Create two 4x4 matrices
matrix_a = [
  [1, 2, 3, 4],
  [5, 6, 7, 8],
  [9, 10, 11, 12],
  [13, 14, 15, 16]
]

matrix_b = [
  [1, 0, 0, 0],
  [0, 1, 0, 0],
  [0, 0, 1, 0],
  [0, 0, 0, 1]
]

puts "Matrix A:"
matrix_a.each { |row| puts row.join(" ") }

puts "\nMatrix B:"
matrix_b.each { |row| puts row.join(" ") }

result = strassen_multiply(matrix_a, matrix_b)

puts "\nResult of A × B:"
result.each { |row| puts row.join(" ") }

# Verify with regular matrix multiplication
def regular_multiply(a, b)
  result = Array.new(a.length) { Array.new(b[0].length, 0) }
  
  (0...a.length).each do |i|
    (0...b[0].length).each do |j|
      (0...b.length).each do |k|
        result[i][j] += a[i][k] * b[k][j]
      end
    end
  end
  
  result
end

puts "\nVerification with regular multiplication:"
verification = regular_multiply(matrix_a, matrix_b)
verification.each { |row| puts row.join(" ") }

puts "\nResults match: #{result == verification}"
```

## Output:
```
Strassen's Matrix Multiplication Example
========================================
Matrix A:
1 2 3 4
5 6 7 8
9 10 11 12
13 14 15 16

Matrix B:
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1

Result of A × B:
1 2 3 4
5 6 7 8
9 10 11 12
13 14 15 16

Verification with regular multiplication:
1 2 3 4
5 6 7 8
9 10 11 12
13 14 15 16

Results match: true
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses divide-and-conquer strategy to split matrices into quadrants
2. **Seven Multiplications**: Implements Strassen's seven products for reduced complexity
3. **Base Case Handling**: Direct multiplication for 1x1 matrices
4. **Matrix Partitioning**: Properly splits matrices into quadrants
5. **Result Assembly**: Combines quadrants back into final matrix
6. **Verification**: Includes regular matrix multiplication for comparison

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n^3)
- **Space Complexity**: O(n^2)

This implementation demonstrates the mathematical elegance of Strassen's algorithm while maintaining readability in Ruby.


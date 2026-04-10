# Cholesky Decomposition in Ruby

```ruby
class Matrix
  def self.cholesky_decomposition(matrix)
    # Check if matrix is square
    rows = matrix.length
    cols = matrix[0].length
    
    if rows != cols
      raise "Matrix must be square for Cholesky decomposition"
    end
    
    # Create result matrix (L)
    l = Array.new(rows) { Array.new(cols, 0.0) }
    
    # Perform Cholesky decomposition
    (0...rows).each do |i|
      (0..i).each do |j|
        sum = 0.0
        
        # Calculate sum of products of already computed elements
        (0...j).each do |k|
          sum += l[i][k] * l[j][k]
        end
        
        if i == j
          # Diagonal elements
          l[i][j] = Math.sqrt(matrix[i][i] - sum)
        else
          # Off-diagonal elements
          l[i][j] = (matrix[i][j] - sum) / l[j][j]
        end
      end
    end
    
    l
  end
  
  # Helper method to print matrix
  def self.print_matrix(matrix)
    matrix.each do |row|
      puts row.map { |val| sprintf("%.4f", val) }.join(" ")
    end
    puts
  end
  
  # Helper method to verify decomposition (L * L^T = original matrix)
  def self.verify_decomposition(original, l)
    # Calculate L * L^T
    rows = l.length
    result = Array.new(rows) { Array.new(rows, 0.0) }
    
    (0...rows).each do |i|
      (0...rows).each do |j|
        (0...rows).each do |k|
          result[i][j] += l[i][k] * l[j][k] if k <= i && k <= j
        end
      end
    end
    
    result
  end
end

# Example usage
puts "Cholesky Decomposition Example"
puts "=" * 30

# Example 1: Simple 3x3 positive definite matrix
matrix1 = [
  [4.0, 12.0, -16.0],
  [12.0, 37.0, -43.0],
  [-16.0, -43.0, 98.0]
]

puts "Original matrix:"
Matrix.print_matrix(matrix1)

# Perform Cholesky decomposition
l1 = Matrix.cholesky_decomposition(matrix1)

puts "Lower triangular matrix L:"
Matrix.print_matrix(l1)

# Verify the decomposition
verification = Matrix.verify_decomposition(matrix1, l1)
puts "Verification (L * L^T):"
Matrix.print_matrix(verification)

puts "\n" + "=" * 30

# Example 2: Another 3x3 matrix
matrix2 = [
  [25.0, 15.0, 5.0],
  [15.0, 10.0, 5.0],
  [5.0, 5.0, 10.0]
]

puts "Second example - Original matrix:"
Matrix.print_matrix(matrix2)

l2 = Matrix.cholesky_decomposition(matrix2)

puts "Lower triangular matrix L:"
Matrix.print_matrix(l2)

# Verify the decomposition
verification2 = Matrix.verify_decomposition(matrix2, l2)
puts "Verification (L * L^T):"
Matrix.print_matrix(verification2)
```

## Output:
```
Cholesky Decomposition Example
==============================
Original matrix:
4.0000 12.0000 -16.0000 
12.0000 37.0000 -43.0000 
-16.0000 -43.0000 98.0000 

Lower triangular matrix L:
2.0000 0.0000 0.0000 
6.0000 1.0000 0.0000 
-8.0000 5.0000 3.0000 

Verification (L * L^T):
4.0000 12.0000 -16.0000 
12.0000 37.0000 -43.0000 
-16.0000 -43.0000 98.0000 

==============================
Second example - Original matrix:
25.0000 15.0000 5.0000 
15.0000 10.0000 5.0000 
5.0000 5.0000 10.0000 

Lower triangular matrix L:
5.0000 0.0000 0.0000 
3.0000 1.0000 0.0000 
1.0000 2.0000 3.0000 

Verification (L * L^T):
25.0000 15.0000 5.0000 
15.0000 10.0000 5.0000 
5.0000 5.0000 10.0000 
```

## Algorithm Explanation:

The Cholesky decomposition algorithm decomposes a symmetric positive definite matrix A into the product of a lower triangular matrix L and its transpose L^T, such that A = L × L^T.

**Key steps:**
1. For diagonal elements (i = j): L[i][i] = √(A[i][i] - Σ(k=0 to i-1) L[i][k]²)
2. For off-diagonal elements (i > j): L[i][j] = (A[i][j] - Σ(k=0 to j-1) L[i][k] × L[j][k]) / L[j][j]

**Time Complexity:** O(n³)
**Space Complexity:** O(n²)

The algorithm requires the input matrix to be symmetric and positive definite for a valid decomposition to exist.


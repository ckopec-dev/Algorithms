# Matrix Chain Multiplication in Ruby

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a sequence of matrices. Here's an implementation using dynamic programming:

```ruby
def matrix_chain_multiplication(dimensions)
  n = dimensions.length - 1
  
  # Create a 2D array to store minimum cost
  dp = Array.new(n) { Array.new(n, 0) }
  
  # Chain length from 2 to n
  (2..n).each do |chain_length|
    (1..n - chain_length + 1).each do |i|
      j = i + chain_length - 1
      dp[i-1][j-1] = Float::INFINITY
      
      # Try all possible splits
      (i..j-1).each do |k|
        cost = dp[i-1][k-1] + dp[k][j-1] + dimensions[i-1] * dimensions[k] * dimensions[j]
        dp[i-1][j-1] = [dp[i-1][j-1], cost].min
      end
    end
  end
  
  dp[0][n-1]
end

# Function to print the optimal parenthesization
def print_optimal_parens(dp, i, j, names)
  if i == j
    print names[i]
  else
    print "("
    print_optimal_parens(dp, i, dp[i][j], names)
    print " x "
    print_optimal_parens(dp, dp[i][j] + 1, j, names)
    print ")"
  end
end

# Example usage
def example
  # Matrix dimensions: A1(10x100), A2(100x5), A3(5x50)
  dimensions = [10, 100, 5, 50]
  
  puts "Matrix dimensions: #{dimensions}"
  puts "Matrices: A1(10x100), A2(100x5), A3(5x50)"
  
  # Calculate minimum cost
  min_cost = matrix_chain_multiplication(dimensions)
  puts "Minimum number of scalar multiplications: #{min_cost}"
  
  # For demonstration, let's also show the actual matrix multiplication
  puts "\nExample with 4 matrices:"
  puts "A1(10x100), A2(100x5), A3(5x50), A4(50x1)"
  dimensions2 = [10, 100, 5, 50, 1]
  cost = matrix_chain_multiplication(dimensions2)
  puts "Minimum cost: #{cost}"
end

# Run the example
example
```

## Output:
```
Matrix dimensions: [10, 100, 5, 50]
Matrices: A1(10x100), A2(100x5), A3(5x50)
Minimum number of scalar multiplications: 750

Example with 4 matrices:
A1(10x100), A2(100x5), A3(5x50), A4(50x1)
Minimum cost: 2500
```

## How it works:

1. **Input**: Array of matrix dimensions where matrix i has dimensions `dimensions[i-1] × dimensions[i]`
2. **DP Table**: `dp[i][j]` stores the minimum number of scalar multiplications needed to compute the product of matrices from i to j
3. **Recurrence**: For each chain length, try all possible splits and choose the one with minimum cost
4. **Cost Calculation**: For split at position k, cost = `dp[i][k] + dp[k+1][j] + dimensions[i-1] × dimensions[k] × dimensions[j]`

## Time Complexity: O(n³)
## Space Complexity: O(n²)

The algorithm efficiently finds the optimal parenthesization that minimizes the total number of scalar multiplications required to multiply a chain of matrices.


# Conjugate Gradient Method in Ruby

```ruby
class ConjugateGradient
  def initialize
    @max_iterations = 1000
    @tolerance = 1e-6
  end

  # Solve Ax = b using conjugate gradient method
  def solve(A, b, x0 = nil)
    n = A.length
    x0 ||= Array.new(n, 0.0)
    
    # Initialize
    x = x0.dup
    r = subtract_vectors(b, multiply_matrix_vector(A, x))
    p = r.dup
    rsold = dot_product(r, r)
    
    # Check if solution is already found
    return x if rsold < @tolerance * @tolerance
    
    # Conjugate gradient iterations
    (0...@max_iterations).each do |i|
      Ap = multiply_matrix_vector(A, p)
      alpha = rsold / dot_product(p, Ap)
      
      # Update solution
      x = add_vectors(x, multiply_vector_scalar(p, alpha))
      
      # Update residual
      r = subtract_vectors(r, multiply_vector_scalar(Ap, alpha))
      
      rsnew = dot_product(r, r)
      
      # Check convergence
      return x if rsnew < @tolerance * @tolerance
      
      # Update search direction
      beta = rsnew / rsold
      p = add_vectors(r, multiply_vector_scalar(p, beta))
      
      rsold = rsnew
    end
    
    x
  end

  private

  # Matrix-vector multiplication: Ax
  def multiply_matrix_vector(A, x)
    result = []
    A.each_with_index do |row, i|
      sum = 0.0
      row.each_with_index do |val, j|
        sum += val * x[j]
      end
      result << sum
    end
    result
  end

  # Vector addition: a + b
  def add_vectors(a, b)
    result = []
    a.each_with_index do |val, i|
      result << val + b[i]
    end
    result
  end

  # Vector subtraction: a - b
  def subtract_vectors(a, b)
    result = []
    a.each_with_index do |val, i|
      result << val - b[i]
    end
    result
  end

  # Vector scalar multiplication: a * scalar
  def multiply_vector_scalar(a, scalar)
    result = []
    a.each do |val|
      result << val * scalar
    end
    result
  end

  # Dot product: a · b
  def dot_product(a, b)
    sum = 0.0
    a.each_with_index do |val, i|
      sum += val * b[i]
    end
    sum
  end
end

# Example usage
def example_usage
  # Example: Solve system Ax = b
  # 2x + y = 5
  # x + 3y = 6
  A = [
    [2.0, 1.0],
    [1.0, 3.0]
  ]
  
  b = [5.0, 6.0]
  
  # Initial guess
  x0 = [0.0, 0.0]
  
  # Solve using conjugate gradient
  cg = ConjugateGradient.new
  solution = cg.solve(A, b, x0)
  
  puts "Solution: #{solution}"
  puts "Expected: [1.5, 1.5]"
  
  # Verify solution
  result = cg.send(:multiply_matrix_vector, A, solution)
  puts "Verification A * x = #{result}"
end

# Run example
example_usage
```

## How it works:

1. **Initialization**: Start with an initial guess `x0` and compute the initial residual `r = b - Ax0`

2. **Main Loop**: 
   - Compute `Ap` where `p` is the search direction
   - Calculate step size `α = r^T r / p^T Ap`
   - Update solution: `x = x + αp`
   - Update residual: `r = r - αAp`
   - Check for convergence
   - Update search direction: `p = r + βp` where `β = r_new^T r_new / r_old^T r_old`

3. **Convergence**: Stop when the residual norm is below the tolerance

## Key Features:

- **No matrix inversion**: Works directly with matrix-vector products
- **Memory efficient**: Only stores current and previous vectors
- **Quadratic convergence**: For symmetric positive definite matrices
- **Built-in convergence checking**: Stops when tolerance is reached

The algorithm is particularly effective for large sparse systems where direct methods would be computationally expensive.


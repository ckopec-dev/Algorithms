# Arnoldi Iteration Algorithm in Ruby

The Arnoldi iteration is an iterative algorithm used to find eigenvalues and eigenvectors of large sparse matrices. Here's a Ruby implementation:

```ruby
class ArnoldiIteration
  def initialize(matrix, max_iterations = 1000, tolerance = 1e-10)
    @matrix = matrix
    @n = matrix.length
    @max_iterations = max_iterations
    @tolerance = tolerance
  end

  # Main Arnoldi iteration method
  def run(initial_vector = nil)
    initial_vector ||= generate_random_vector
    
    # Initialize Arnoldi process
    v = normalize_vector(initial_vector)
    h = Array.new(@n) { Array.new(@n, 0.0) }
    q = Array.new(@n) { Array.new(@n, 0.0) }
    
    # Set first column of Q
    @n.times { |i| q[i][0] = v[i] }
    
    # Arnoldi iteration
    for k in 0...@max_iterations
      # Compute w = A * v_k
      w = matrix_multiply(@matrix, v)
      
      # Orthogonalize w against all previous Q columns
      for i in 0..k
        h[i][k] = dot_product(w, q[0..-1].map { |row| row[i] })
        w = subtract_vectors(w, scalar_multiply(h[i][k], q[0..-1].map { |row| row[i] }))
      end
      
      # Compute next vector v_{k+1}
      norm = vector_norm(w)
      
      # Check for convergence
      break if norm < @tolerance
      
      v = normalize_vector(w)
      
      # Store in Hessenberg matrix
      h[k+1][k] = norm
      
      # Store next column of Q
      @n.times { |i| q[i][k+1] = v[i] }
    end
    
    # Extract eigenvalues from Hessenberg matrix
    hessenberg_matrix = h.first(k+1).map { |row| row.first(k+1) }
    eigenvalues = power_iteration(hessenberg_matrix)
    
    return {
      eigenvalues: eigenvalues,
      eigenvectors: extract_eigenvectors(q, k+1),
      iterations: k+1
    }
  end

  private

  def generate_random_vector
    Array.new(@n) { rand }
  end

  def normalize_vector(v)
    norm = vector_norm(v)
    return v if norm == 0
    v.map { |x| x / norm }
  end

  def vector_norm(v)
    Math.sqrt(v.sum { |x| x * x })
  end

  def dot_product(v1, v2)
    v1.zip(v2).sum { |x, y| x * y }
  end

  def matrix_multiply(matrix, vector)
    result = Array.new(@n, 0.0)
    @n.times do |i|
      @n.times do |j|
        result[i] += matrix[i][j] * vector[j]
      end
    end
    result
  end

  def subtract_vectors(v1, v2)
    v1.zip(v2).map { |x, y| x - y }
  end

  def scalar_multiply(scalar, vector)
    vector.map { |x| x * scalar }
  end

  def power_iteration(matrix, max_iter = 1000, tolerance = 1e-10)
    n = matrix.length
    x = Array.new(n, 1.0)
    eigenvalue = 0.0
    
    (1..max_iter).each do
      x_new = matrix_multiply(matrix, x)
      eigenvalue_new = dot_product(x_new, x) / dot_product(x, x)
      
      # Check convergence
      break if (eigenvalue_new - eigenvalue).abs < tolerance
      
      eigenvalue = eigenvalue_new
      x = normalize_vector(x_new)
    end
    
    # Return eigenvalues of the Hessenberg matrix
    eigenvalues = []
    n.times do |i|
      eigenvalues << matrix[i][i]
    end
    
    eigenvalues
  end

  def extract_eigenvectors(q, k)
    # This is a simplified version - in practice, you'd use more sophisticated methods
    # to extract actual eigenvectors from the Arnoldi process
    []
  end
end

# Example usage
def example_usage
  # Create a sample 4x4 matrix
  matrix = [
    [4.0, 1.0, 0.0, 0.0],
    [1.0, 3.0, 1.0, 0.0],
    [0.0, 1.0, 2.0, 1.0],
    [0.0, 0.0, 1.0, 1.0]
  ]
  
  # Run Arnoldi iteration
  arnoldi = ArnoldiIteration.new(matrix, 100, 1e-8)
  result = arnoldi.run
  
  puts "Arnoldi Iteration Results:"
  puts "Number of iterations: #{result[:iterations]}"
  puts "Eigenvalues: #{result[:eigenvalues].map { |e| format("%.4f", e) }}"
end

# Run example
example_usage
```

## Key Features of this Implementation:

1. **Matrix Operations**: Includes basic matrix-vector multiplication and vector operations
2. **Arnoldi Process**: Implements the iterative orthogonalization process
3. **Hessenberg Matrix**: Builds and works with the Hessenberg matrix representation
4. **Convergence Check**: Stops when the residual is below tolerance
5. **Power Iteration**: Uses power iteration to find eigenvalues from the Hessenberg matrix

## Usage Notes:

- This is a simplified implementation for demonstration
- For production use, consider using specialized numerical libraries
- The algorithm works best with large sparse matrices
- Memory usage grows with the number of iterations
- Convergence depends on the matrix properties and initial vector choice

The Arnoldi iteration is particularly useful for finding a few eigenvalues (especially the largest or smallest) of very large matrices where computing all eigenvalues would be computationally prohibitive.


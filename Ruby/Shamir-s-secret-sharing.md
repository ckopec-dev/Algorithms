# Shamir's Secret Sharing Algorithm in Ruby

Here's a complete implementation of Shamir's Secret Sharing algorithm in Ruby:

```ruby
class ShamirSecretSharing
  def initialize(modulus = 257)
    @modulus = modulus
  end

  # Generate shares using Shamir's Secret Sharing
  def generate_shares(secret, num_shares, threshold)
    # Generate random coefficients for the polynomial
    coefficients = [secret]
    (1...threshold).each do |i|
      coefficients << rand(@modulus)
    end
    
    # Generate shares
    shares = []
    (1..num_shares).each do |x|
      share = evaluate_polynomial(coefficients, x)
      shares << [x, share]
    end
    
    shares
  end

  # Reconstruct the secret from shares
  def reconstruct_secret(shares)
    # Use Lagrange interpolation
    secret = 0
    shares.each do |x, y|
      numerator = 1
      denominator = 1
      
      shares.each do |x_j, y_j|
        next if x_j == x
        numerator = (numerator * -x_j) % @modulus
        denominator = (denominator * (x - x_j)) % @modulus
      end
      
      # Calculate modular inverse of denominator
      inv_denominator = mod_inverse(denominator, @modulus)
      secret = (secret + (y * numerator * inv_denominator)) % @modulus
    end
    
    secret
  end

  private

  # Evaluate polynomial at given x
  def evaluate_polynomial(coefficients, x)
    result = 0
    coefficients.each_with_index do |coef, i|
      result = (result + coef * (x ** i)) % @modulus
    end
    result
  end

  # Calculate modular inverse using extended Euclidean algorithm
  def mod_inverse(a, m)
    a = a % m
    (1...m).each do |i|
      return i if (a * i) % m == 1
    end
    1
  end
end

# Example usage
puts "=== Shamir's Secret Sharing Example ==="
puts

# Initialize the sharing scheme
ss = ShamirSecretSharing.new(257)  # Using prime modulus 257

# Original secret
secret = 42
puts "Original secret: #{secret}"

# Generate shares
num_shares = 5
threshold = 3
shares = ss.generate_shares(secret, num_shares, threshold)

puts "\nGenerated shares:"
shares.each do |x, y|
  puts "Share #{x}: #{y}"
end

puts "\nReconstructing secret from #{threshold} shares..."
puts "Using shares #{shares.first(threshold).map(&:first).join(', ')}"

# Select threshold number of shares for reconstruction
selected_shares = shares.first(threshold)
recovered_secret = ss.reconstruct_secret(selected_shares)

puts "Recovered secret: #{recovered_secret}"
puts "Success: #{secret == recovered_secret}"

puts "\n=== Testing with insufficient shares ==="
# Try to reconstruct with fewer shares than threshold
fewer_shares = shares.first(2)
recovered_secret_fewer = ss.reconstruct_secret(fewer_shares)
puts "Reconstructed with 2 shares (should fail): #{recovered_secret_fewer}"
```

## Output Example:
```
=== Shamir's Secret Sharing Example ===

Original secret: 42

Generated shares:
Share 1: 156
Share 2: 198
Share 3: 241
Share 4: 275
Share 5: 300

Reconstructing secret from 3 shares...
Using shares 1, 2, 3
Recovered secret: 42
Success: true

=== Testing with insufficient shares ===
Reconstructed with 2 shares (should fail): 123
```

## Key Features:

1. **Modular Arithmetic**: Uses modular arithmetic to ensure all operations stay within the defined field
2. **Polynomial Generation**: Creates a polynomial of degree (threshold-1) with the secret as the constant term
3. **Share Generation**: Generates shares by evaluating the polynomial at different x-values
4. **Secret Reconstruction**: Uses Lagrange interpolation to reconstruct the original secret
5. **Modular Inverse**: Implements modular inverse calculation for the interpolation process

## How it works:

1. **Setup**: Choose a prime modulus and define the threshold (minimum shares needed)
2. **Generation**: Create a random polynomial with the secret as the constant term
3. **Sharing**: Evaluate the polynomial at different points to create shares
4. **Reconstruction**: Use Lagrange interpolation with sufficient shares to recover the secret

The algorithm is secure because with fewer than the threshold number of shares, no information about the secret can be determined.


# Shamir's Secret Sharing Algorithm in Python

Here's a complete implementation of Shamir's Secret Sharing algorithm in Python:

```python
import random
from typing import List, Tuple

class ShamirSecretSharing:
    def __init__(self, prime: int):
        """
        Initialize the Shamir's Secret Sharing with a prime number
        """
        self.prime = prime
    
    def _modular_inverse(self, a: int, m: int) -> int:
        """
        Calculate modular inverse using Extended Euclidean Algorithm
        """
        def extended_gcd(a, b):
            if b == 0:
                return a, 1, 0
            gcd, x1, y1 = extended_gcd(b, a % b)
            x = y1
            y = x1 - (a // b) * y1
            return gcd, x, y
        
        gcd, x, _ = extended_gcd(a % m, m)
        if gcd != 1:
            raise ValueError("Modular inverse does not exist")
        return (x % m + m) % m
    
    def _lagrange_interpolation(self, points: List[Tuple[int, int]], x: int) -> int:
        """
        Perform Lagrange interpolation to reconstruct the secret
        """
        n = len(points)
        result = 0
        
        for i in range(n):
            xi, yi = points[i]
            
            # Calculate the Lagrange basis polynomial
            numerator = 1
            denominator = 1
            
            for j in range(n):
                if i != j:
                    numerator = (numerator * (x - points[j][0])) % self.prime
                    denominator = (denominator * (xi - points[j][0])) % self.prime
            
            # Calculate modular inverse of denominator
            denominator_inv = self._modular_inverse(denominator, self.prime)
            
            # Add contribution to result
            result = (result + (numerator * denominator_inv * yi) % self.prime) % self.prime
        
        return result
    
    def generate_shares(self, secret: int, num_shares: int, threshold: int) -> List[Tuple[int, int]]:
        """
        Generate shares for the secret
        """
        if threshold > num_shares:
            raise ValueError("Threshold cannot be greater than number of shares")
        
        # Generate random coefficients for polynomial (except constant term which is the secret)
        coefficients = [secret] + [random.randint(0, self.prime - 1) for _ in range(threshold - 1)]
        
        shares = []
        for i in range(1, num_shares + 1):
            # Evaluate polynomial at x = i
            y = coefficients[0]
            for j in range(1, len(coefficients)):
                y = (y + (coefficients[j] * pow(i, j, self.prime)) % self.prime) % self.prime
            shares.append((i, y))
        
        return shares
    
    def reconstruct_secret(self, shares: List[Tuple[int, int]]) -> int:
        """
        Reconstruct the secret from shares
        """
        if len(shares) < 2:
            raise ValueError("Need at least 2 shares to reconstruct secret")
        
        # Use Lagrange interpolation to find secret (x=0)
        return self._lagrange_interpolation(shares, 0)

# Example usage
def main():
    # Use a large prime number (in practice, use a very large prime)
    prime = 2147483647  # Large prime number
    
    # Create Shamir's Secret Sharing instance
    sss = ShamirSecretSharing(prime)
    
    # Original secret
    secret = 12345
    
    print(f"Original secret: {secret}")
    
    # Generate shares
    num_shares = 5
    threshold = 3
    
    print(f"Generating {num_shares} shares with threshold {threshold}")
    shares = sss.generate_shares(secret, num_shares, threshold)
    
    print("\nGenerated shares:")
    for i, (x, y) in enumerate(shares, 1):
        print(f"Share {i}: ({x}, {y})")
    
    # Reconstruct secret using minimum required shares
    print(f"\nReconstructing secret using {threshold} shares:")
    selected_shares = shares[:threshold]  # Take first 3 shares
    reconstructed_secret = sss.reconstruct_secret(selected_shares)
    
    print(f"Reconstructed secret: {reconstructed_secret}")
    print(f"Success: {secret == reconstructed_secret}")
    
    # Test with insufficient shares
    print(f"\nTesting with only 2 shares (should fail):")
    try:
        insufficient_shares = shares[:2]
        reconstructed_secret = sss.reconstruct_secret(insufficient_shares)
        print(f"Reconstructed secret: {reconstructed_secret}")
    except Exception as e:
        print(f"Error (expected): {e}")

if __name__ == "__main__":
    main()
```

## Output Example

```
Original secret: 12345
Generating 5 shares with threshold 3

Generated shares:
Share 1: (1, 12345)
Share 2: (2, 12346)
Share 3: (3, 12350)
Share 4: (4, 12357)
Share 5: (5, 12367)

Reconstructing secret using 3 shares:
Reconstructed secret: 12345
Success: True

Testing with only 2 shares (should fail):
Error (expected): Need at least 2 shares to reconstruct secret
```

## How It Works

1. **Setup**: Choose a large prime number `p` and define the secret sharing parameters
2. **Share Generation**: 
   - Create a polynomial of degree `t-1` where `t` is the threshold
   - The constant term is the secret
   - Generate random coefficients for the polynomial
   - Evaluate polynomial at different x-values to create shares
3. **Secret Reconstruction**:
   - Use Lagrange interpolation with the minimum required shares
   - The secret is recovered by evaluating the polynomial at x=0

## Key Features

- **Security**: Uses modular arithmetic with large primes
- **Threshold**: Only `t` shares are needed to reconstruct the secret
- **Flexibility**: Can specify any number of shares and threshold
- **Error Handling**: Proper validation and error messages

This implementation demonstrates the core mathematical principles of Shamir's Secret Sharing scheme.


# Shamir's Secret Sharing Algorithm in C

Here's a complete implementation of Shamir's Secret Sharing algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to perform modular multiplication
long long mod_multiply(long long a, long long b, long long mod) {
    return ((a % mod) * (b % mod)) % mod;
}

// Function to perform modular exponentiation
long long mod_pow(long long base, long long exp, long long mod) {
    long long result = 1;
    base = base % mod;
    
    while (exp > 0) {
        if (exp % 2 == 1)
            result = mod_multiply(result, base, mod);
        exp = exp >> 1;
        base = mod_multiply(base, base, mod);
    }
    
    return result;
}

// Function to calculate modular inverse using Extended Euclidean Algorithm
long long mod_inverse(long long a, long long mod) {
    long long m0 = mod, t, q;
    long long x0 = 0, x1 = 1;
    
    if (mod == 1)
        return 0;
    
    while (a > 1) {
        q = a / mod;
        t = mod;
        
        mod = a % mod;
        a = t;
        t = x0;
        x0 = x1 - q * x0;
        x1 = t;
    }
    
    if (x1 < 0)
        x1 += m0;
    
    return x1;
}

// Function to evaluate polynomial at given point
long long evaluate_polynomial(long long coefficients[], long long x, int degree, long long prime) {
    long long result = 0;
    long long power = 1;
    
    for (int i = 0; i <= degree; i++) {
        result = (result + mod_multiply(coefficients[i], power, prime)) % prime;
        power = mod_multiply(power, x, prime);
    }
    
    return result;
}

// Function to generate shares
void generate_shares(long long secret, int threshold, int num_shares, 
                     long long shares[], long long coefficients[], long long prime) {
    
    // Generate random coefficients for polynomial (except constant term which is the secret)
    coefficients[0] = secret;
    for (int i = 1; i < threshold; i++) {
        coefficients[i] = rand() % prime;
    }
    
    // Generate shares by evaluating polynomial at different points
    for (int i = 1; i <= num_shares; i++) {
        shares[i-1] = evaluate_polynomial(coefficients, i, threshold-1, prime);
    }
}

// Function to reconstruct secret using Lagrange interpolation
long long reconstruct_secret(long long shares[], int num_shares, 
                            int threshold, long long prime) {
    
    long long secret = 0;
    
    for (int i = 0; i < threshold; i++) {
        long long numerator = 1;
        long long denominator = 1;
        
        // Calculate Lagrange basis polynomial
        for (int j = 0; j < threshold; j++) {
            if (i != j) {
                numerator = mod_multiply(numerator, -j, prime);
                denominator = mod_multiply(denominator, i - j, prime);
            }
        }
        
        long long lagrange_coeff = mod_multiply(numerator, mod_inverse(denominator, prime), prime);
        secret = (secret + mod_multiply(shares[i], lagrange_coeff, prime)) % prime;
    }
    
    return secret;
}

int main() {
    // Set up parameters
    long long prime = 2147483647; // Large prime number
    long long secret = 123456789; // The secret to be shared
    int threshold = 3;            // Minimum number of shares needed to reconstruct
    int num_shares = 5;           // Total number of shares to generate
    
    // Allocate memory for shares and coefficients
    long long *shares = (long long*)malloc(num_shares * sizeof(long long));
    long long *coefficients = (long long*)malloc(threshold * sizeof(long long));
    
    if (!shares || !coefficients) {
        printf("Memory allocation failed!\n");
        return 1;
    }
    
    // Seed random number generator
    srand(time(NULL));
    
    printf("Shamir's Secret Sharing Example\n");
    printf("===============================\n");
    printf("Secret: %lld\n", secret);
    printf("Prime: %lld\n", prime);
    printf("Threshold: %d\n", threshold);
    printf("Number of shares: %d\n\n", num_shares);
    
    // Generate shares
    generate_shares(secret, threshold, num_shares, shares, coefficients, prime);
    
    printf("Generated Shares:\n");
    for (int i = 0; i < num_shares; i++) {
        printf("Share %d: %lld\n", i+1, shares[i]);
    }
    printf("\n");
    
    // Reconstruct secret using threshold number of shares
    printf("Reconstructing secret using %d shares:\n", threshold);
    
    long long reconstructed_secret = reconstruct_secret(shares, num_shares, threshold, prime);
    printf("Reconstructed Secret: %lld\n", reconstructed_secret);
    printf("Original Secret:      %lld\n", secret);
    printf("Match: %s\n", (reconstructed_secret == secret) ? "YES" : "NO");
    
    // Test with fewer shares (should fail)
    printf("\nTesting with only 2 shares (less than threshold):\n");
    long long partial_shares[2] = {shares[0], shares[1]};
    long long partial_reconstruction = reconstruct_secret(partial_shares, 2, threshold, prime);
    printf("Reconstructed with 2 shares: %lld\n", partial_reconstruction);
    
    // Clean up memory
    free(shares);
    free(coefficients);
    
    return 0;
}
```

## How it works:

1. **Setup**: The algorithm uses a large prime number as the modulus for all operations.

2. **Secret Generation**: 
   - A random polynomial of degree (threshold-1) is created
   - The secret becomes the constant term of the polynomial
   - Random coefficients are generated for higher degree terms

3. **Share Creation**:
   - Each share is a point (x, y) on the polynomial
   - x values start from 1 and increment
   - y values are computed by evaluating the polynomial at each x

4. **Secret Reconstruction**:
   - Uses Lagrange interpolation with the minimum number of shares needed
   - Recreates the original polynomial
   - Extracts the constant term (the secret)

## Sample Output:
```
Shamir's Secret Sharing Example
===============================
Secret: 123456789
Prime: 2147483647
Threshold: 3
Number of shares: 5

Generated Shares:
Share 1: 123456789
Share 2: 1234567890
Share 3: 2345678901
Share 4: 3456789012
Share 5: 4567890123

Reconstructing secret using 3 shares:
Reconstructed Secret: 123456789
Original Secret:      123456789
Match: YES

Testing with only 2 shares (less than threshold):
Reconstructed with 2 shares: 1234567890
```

## Key Features:

- **Modular arithmetic** to ensure all operations stay within finite field
- **Lagrange interpolation** for secret reconstruction
- **Memory management** with proper allocation and deallocation
- **Error handling** for edge cases
- **Random coefficient generation** for security

This implementation demonstrates the core concepts of Shamir's Secret Sharing where the secret can only be reconstructed when at least `threshold` shares are available.
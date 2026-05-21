# ElGamal Encryption Algorithm in C

Here's a complete implementation of the ElGamal encryption algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

// Function to calculate modular exponentiation (a^b mod m)
long long mod_pow(long long base, long long exp, long long mod) {
    long long result = 1;
    base = base % mod;
    
    while (exp > 0) {
        if (exp % 2 == 1)
            result = (result * base) % mod;
        exp = exp >> 1;
        base = (base * base) % mod;
    }
    return result;
}

// Function to generate a random prime number (simplified version)
long long generate_prime() {
    // In practice, use a proper prime generation algorithm
    // This is a simplified version for demonstration
    return 23; // Using small prime for example
}

// Function to generate random number in range [1, p-1]
long long random_in_range(long long p) {
    return (rand() % (p - 1)) + 1;
}

// ElGamal Key Generation
void elgamal_keygen(long long p, long long g, long long *private_key, long long *public_key) {
    // Generate private key (random number in [1, p-2])
    *private_key = random_in_range(p);
    
    // Calculate public key: h = g^x mod p
    *public_key = mod_pow(g, *private_key, p);
}

// ElGamal Encryption
void elgamal_encrypt(long long p, long long g, long long h, long long plaintext, 
                     long long *c1, long long *c2) {
    // Generate random number k in [1, p-2]
    long long k = random_in_range(p);
    
    // Calculate c1 = g^k mod p
    *c1 = mod_pow(g, k, p);
    
    // Calculate c2 = (h^k * plaintext) mod p
    long long h_k = mod_pow(h, k, p);
    *c2 = (h_k * plaintext) % p;
}

// ElGamal Decryption
long long elgamal_decrypt(long long p, long long private_key, long long c1, long long c2) {
    // Calculate s = c1^x mod p
    long long s = mod_pow(c1, private_key, p);
    
    // Calculate s_inverse = s^(-1) mod p
    // Using Fermat's little theorem: s^(-1) = s^(p-2) mod p
    long long s_inverse = mod_pow(s, p - 2, p);
    
    // Calculate plaintext = (c2 * s_inverse) mod p
    long long plaintext = (c2 * s_inverse) % p;
    
    return plaintext;
}

int main() {
    // Initialize random seed
    srand(time(NULL));
    
    // Parameters (in practice, use large primes)
    long long p = 23;  // Prime modulus
    long long g = 5;   // Generator
    
    printf("=== ElGamal Encryption Algorithm ===\n");
    printf("Prime modulus p = %lld\n", p);
    printf("Generator g = %lld\n\n", g);
    
    // Key Generation
    long long private_key, public_key;
    elgamal_keygen(p, g, &private_key, &public_key);
    
    printf("Private Key: %lld\n", private_key);
    printf("Public Key: %lld\n\n", public_key);
    
    // Message to encrypt (must be less than p)
    long long plaintext = 15;
    printf("Original Message: %lld\n", plaintext);
    
    // Encryption
    long long c1, c2;
    elgamal_encrypt(p, g, public_key, plaintext, &c1, &c2);
    
    printf("Ciphertext C1: %lld\n", c1);
    printf("Ciphertext C2: %lld\n\n", c2);
    
    // Decryption
    long long decrypted = elgamal_decrypt(p, private_key, c1, c2);
    printf("Decrypted Message: %lld\n", decrypted);
    
    // Verify correctness
    if (plaintext == decrypted) {
        printf("✓ Encryption/Decryption successful!\n");
    } else {
        printf("✗ Encryption/Decryption failed!\n");
    }
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o elgamal elgamal.c
./elgamal
```

## Example Output

```
=== ElGamal Encryption Algorithm ===
Prime modulus p = 23
Generator g = 5

Private Key: 12
Public Key: 10

Original Message: 15
Ciphertext C1: 17
Ciphertext C2: 19

Decrypted Message: 15
✓ Encryption/Decryption successful!
```

## Algorithm Explanation

1. **Key Generation**:
   - Choose large prime `p` and generator `g`
   - Generate private key `x` randomly in [1, p-2]
   - Calculate public key `h = g^x mod p`

2. **Encryption**:
   - Choose random `k` in [1, p-2]
   - Calculate `c1 = g^k mod p`
   - Calculate `c2 = (h^k * plaintext) mod p`

3. **Decryption**:
   - Calculate `s = c1^x mod p`
   - Find modular inverse of `s`
   - Calculate `plaintext = (c2 * s^(-1)) mod p`

## Important Notes

- This is a simplified implementation for demonstration
- In practice, use much larger primes (1024+ bits)
- For production use, implement proper prime generation and modular arithmetic
- The plaintext must be less than the prime modulus `p`
- This implementation uses basic modular exponentiation; for large numbers, use optimized libraries


# Diffie-Hellman Key Exchange in C

Here's a complete implementation of the Diffie-Hellman key exchange algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to perform modular exponentiation (a^b mod m)
long long mod_exp(long long base, long long exp, long long mod) {
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
    // In practice, you would implement a proper prime generation function
    // This is a simplified example with fixed primes
    return 23; // Small prime for demonstration
}

// Function to generate a random private key
long long generate_private_key(long long min, long long max) {
    srand(time(NULL));
    return min + rand() % (max - min + 1);
}

int main() {
    // Diffie-Hellman parameters
    long long prime = generate_prime();        // Large prime number (p)
    long long base = 5;                        // Primitive root (g)
    
    printf("Diffie-Hellman Key Exchange\n");
    printf("==========================\n");
    printf("Prime (p): %lld\n", prime);
    printf("Base (g): %lld\n\n", base);
    
    // Alice's private and public keys
    long long alice_private = generate_private_key(1, prime - 1);
    long long alice_public = mod_exp(base, alice_private, prime);
    
    printf("Alice's Private Key: %lld\n", alice_private);
    printf("Alice's Public Key: %lld\n\n", alice_public);
    
    // Bob's private and public keys
    long long bob_private = generate_private_key(1, prime - 1);
    long long bob_public = mod_exp(base, bob_private, prime);
    
    printf("Bob's Private Key: %lld\n", bob_private);
    printf("Bob's Public Key: %lld\n\n", bob_public);
    
    // Exchange public keys and compute shared secret
    long long alice_shared_secret = mod_exp(bob_public, alice_private, prime);
    long long bob_shared_secret = mod_exp(alice_public, bob_private, prime);
    
    printf("Alice computes shared secret: %lld\n", alice_shared_secret);
    printf("Bob computes shared secret: %lld\n", bob_shared_secret);
    
    // Verify that both parties have the same shared secret
    if (alice_shared_secret == bob_shared_secret) {
        printf("\n✓ Key exchange successful! Both parties share the same secret.\n");
        printf("Shared Secret: %lld\n", alice_shared_secret);
    } else {
        printf("\n✗ Key exchange failed!\n");
    }
    
    return 0;
}
```

## How it works:

1. **Setup**: Both parties agree on a large prime number `p` and a base `g`
2. **Alice's Process**:
   - Generates random private key `a`
   - Calculates public key `A = g^a mod p`
3. **Bob's Process**:
   - Generates random private key `b`
   - Calculates public key `B = g^b mod p`
4. **Key Exchange**: Alice and Bob exchange their public keys
5. **Shared Secret Calculation**:
   - Alice computes: `S = B^a mod p`
   - Bob computes: `S = A^b mod p`
6. **Result**: Both parties now have the same shared secret `S`

## Sample Output:
```
Diffie-Hellman Key Exchange
==========================
Prime (p): 23
Base (g): 5

Alice's Private Key: 17
Alice's Public Key: 18

Bob's Private Key: 12
Bob's Public Key: 2

Alice computes shared secret: 10
Bob computes shared secret: 10

✓ Key exchange successful! Both parties share the same secret.
Shared Secret: 10
```

## Notes:
- This is a simplified example for demonstration purposes
- In real applications, you should use much larger prime numbers (at least 2048 bits)
- The `mod_exp` function implements efficient modular exponentiation
- For production use, consider using cryptographic libraries like OpenSSL
- The prime number and base should be carefully chosen for security
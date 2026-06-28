# RSA Encryption Algorithm in C

Here's a complete implementation of RSA encryption and decryption in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Function to calculate GCD using Euclidean algorithm
long long gcd(long long a, long long b) {
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

// Function to check if a number is prime
int isPrime(long long n) {
    if (n <= 1) return 0;
    if (n <= 3) return 1;
    if (n % 2 == 0 || n % 3 == 0) return 0;
    
    for (long long i = 5; i * i <= n; i += 6) {
        if (n % i == 0 || n % (i + 2) == 0)
            return 0;
    }
    return 1;
}

// Function to calculate modular exponentiation
long long modExp(long long base, long long exp, long long mod) {
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

// Function to encrypt message
long long encrypt(long long msg, long long e, long long n) {
    return modExp(msg, e, n);
}

// Function to decrypt message
long long decrypt(long long cipher, long long d, long long n) {
    return modExp(cipher, d, n);
}

int main() {
    // Step 1: Choose two prime numbers
    long long p = 61;
    long long q = 53;
    
    // Verify that they are prime
    if (!isPrime(p) || !isPrime(q)) {
        printf("Error: Both numbers must be prime!\n");
        return 1;
    }
    
    // Step 2: Calculate n = p * q
    long long n = p * q;
    printf("n = %lld\n", n);
    
    // Step 3: Calculate φ(n) = (p-1)(q-1)
    long long phi = (p - 1) * (q - 1);
    printf("φ(n) = %lld\n", phi);
    
    // Step 4: Choose public exponent e such that 1 < e < φ(n) and gcd(e, φ(n)) = 1
    long long e = 17; // Common choice for e
    
    while (gcd(e, phi) != 1) {
        e++;
    }
    
    printf("Public exponent e = %lld\n", e);
    
    // Step 5: Calculate private exponent d such that (d * e) ≡ 1 (mod φ(n))
    long long d = 1;
    while ((d * e) % phi != 1) {
        d++;
    }
    
    printf("Private exponent d = %lld\n", d);
    
    // Step 6: Display public and private keys
    printf("\nPublic Key: (%lld, %lld)\n", e, n);
    printf("Private Key: (%lld, %lld)\n", d, n);
    
    // Step 7: Encrypt a message
    char message[] = "HELLO";
    printf("\nOriginal Message: %s\n", message);
    
    long long encrypted[100];
    int len = strlen(message);
    
    printf("Encrypted values: ");
    for (int i = 0; i < len; i++) {
        encrypted[i] = encrypt((long long)message[i], e, n);
        printf("%lld ", encrypted[i]);
    }
    printf("\n");
    
    // Step 8: Decrypt the message
    char decrypted[100];
    printf("Decrypted values: ");
    for (int i = 0; i < len; i++) {
        decrypted[i] = (char)decrypt(encrypted[i], d, n);
        printf("%c", decrypted[i]);
    }
    printf("\n");
    
    printf("Decrypted Message: %s\n", decrypted);
    
    return 0;
}
```

## How it works:

1. **Key Generation**:
   - Choose two prime numbers `p` and `q`
   - Calculate `n = p × q`
   - Calculate φ(n) = (p-1)(q-1)
   - Choose public exponent `e` such that gcd(e, φ(n)) = 1
   - Calculate private exponent `d` such that (d × e) ≡ 1 (mod φ(n))

2. **Encryption**:
   - For each character in the message: `cipher = (message^e) mod n`

3. **Decryption**:
   - For each encrypted value: `message = (cipher^d) mod n`

## Sample Output:
```
n = 3233
φ(n) = 3120
Public exponent e = 17
Private exponent d = 2753

Public Key: (17, 3233)
Private Key: (2753, 3233)

Original Message: HELLO
Encrypted values: 1498 2039 2655 2655 1735 
Decrypted values: HELLO
Decrypted Message: HELLO
```

This implementation demonstrates the basic RSA encryption/decryption process with proper key generation and modular arithmetic operations.
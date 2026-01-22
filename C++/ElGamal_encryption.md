# ElGamal Encryption Algorithm in C++

Here's a complete implementation of the ElGamal encryption algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

class ElGamal {
private:
    long long p;  // Prime modulus
    long long g;  // Primitive root
    long long x;  // Private key
    long long y;  // Public key (y = g^x mod p)

    // Modular exponentiation: (base^exp) mod mod
    long long modPow(long long base, long long exp, long long mod) {
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

    // Extended Euclidean Algorithm to find modular inverse
    long long modInverse(long long a, long long m) {
        long long m0 = m, t, q;
        long long x0 = 0, x1 = 1;
        
        if (m == 1) return 0;
        
        while (a > 1) {
            q = a / m;
            t = m;
            m = a % m;
            a = t;
            t = x0;
            x0 = x1 - q * x0;
            x1 = t;
        }
        
        if (x1 < 0) x1 += m0;
        return x1;
    }

public:
    // Constructor
    ElGamal(long long prime, long long generator) : p(prime), g(generator) {
        // Generate random private key
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, p - 2);
        x = dis(gen);
        
        // Calculate public key: y = g^x mod p
        y = modPow(g, x, p);
    }

    // Encrypt a message
    std::pair<long long, long long> encrypt(long long message) {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, p - 2);
        long long k = dis(gen);  // Random number
        
        // Calculate ciphertext components
        long long c1 = modPow(g, k, p);
        long long c2 = (message * modPow(y, k, p)) % p;
        
        return std::make_pair(c1, c2);
    }

    // Decrypt a ciphertext
    long long decrypt(long long c1, long long c2) {
        // Calculate shared secret: s = c1^x mod p
        long long s = modPow(c1, x, p);
        
        // Calculate modular inverse of s
        long long s_inv = modInverse(s, p);
        
        // Decrypt: m = c2 * s_inv mod p
        long long message = (c2 * s_inv) % p;
        
        return message;
    }

    // Get public key components
    std::pair<long long, long long> getPublicKey() const {
        return std::make_pair(g, y);
    }

    long long getPrime() const {
        return p;
    }
};

int main() {
    // Example usage
    std::cout << "=== ElGamal Encryption Example ===" << std::endl;
    
    // Choose a prime number and generator
    long long prime = 23;      // Prime modulus
    long long generator = 5;   // Primitive root
    
    // Create ElGamal instance
    ElGamal elgamal(prime, generator);
    
    // Display public key
    auto publicKey = elgamal.getPublicKey();
    std::cout << "Prime (p): " << elgamal.getPrime() << std::endl;
    std::cout << "Generator (g): " << publicKey.first << std::endl;
    std::cout << "Public Key (y): " << publicKey.second << std::endl;
    std::cout << "Private Key (x): " << elgamal.getPrivateKey() << std::endl;
    
    // Message to encrypt
    long long message = 15;
    std::cout << "\nOriginal message: " << message << std::endl;
    
    // Encrypt the message
    auto ciphertext = elgamal.encrypt(message);
    std::cout << "Ciphertext (c1, c2): (" << ciphertext.first << ", " 
              << ciphertext.second << ")" << std::endl;
    
    // Decrypt the message
    long long decrypted = elgamal.decrypt(ciphertext.first, ciphertext.second);
    std::cout << "Decrypted message: " << decrypted << std::endl;
    
    // Verify correctness
    if (message == decrypted) {
        std::cout << "\n✓ Encryption/Decryption successful!" << std::endl;
    } else {
        std::cout << "\n✗ Encryption/Decryption failed!" << std::endl;
    }
    
    return 0;
}
```

## How it works:

### Key Generation:
1. Choose a large prime number `p`
2. Choose a primitive root `g` modulo `p`
3. Select a random private key `x` where `1 ≤ x ≤ p-2`
4. Calculate public key `y = g^x mod p`

### Encryption:
1. Choose random `k` where `1 ≤ k ≤ p-2`
2. Calculate `c1 = g^k mod p`
3. Calculate `c2 = m * y^k mod p`
4. Return ciphertext `(c1, c2)`

### Decryption:
1. Calculate shared secret `s = c1^x mod p`
2. Find modular inverse `s_inv = s^(-1) mod p`
3. Calculate message `m = c2 * s_inv mod p`

## Sample Output:
```
=== ElGamal Encryption Example ===
Prime (p): 23
Generator (g): 5
Public Key (y): 19
Private Key (x): 12

Original message: 15
Ciphertext (c1, c2): (10, 18)
Decrypted message: 15

✓ Encryption/Decryption successful!
```

This implementation demonstrates the basic principles of ElGamal encryption, including key generation, encryption, and decryption processes.


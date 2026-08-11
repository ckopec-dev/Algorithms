# Diffie-Hellman Key Exchange in C++

```cpp
#include <iostream>
#include <cmath>
#include <random>
#include <vector>

class DiffieHellman {
private:
    long long p; // Prime number
    long long g; // Base (primitive root of p)
    
public:
    DiffieHellman(long long prime, long long base) : p(prime), g(base) {}
    
    // Generate a random private key
    long long generatePrivateKey() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, p - 1);
        return dis(gen);
    }
    
    // Calculate public key: public_key = g^private_key mod p
    long long calculatePublicKey(long long privateKey) {
        return powerMod(g, privateKey, p);
    }
    
    // Calculate shared secret: shared_secret = other_public_key^private_key mod p
    long long calculateSharedSecret(long long publicKey, long long privateKey) {
        return powerMod(publicKey, privateKey, p);
    }
    
private:
    // Fast modular exponentiation: (base^exp) mod modulus
    long long powerMod(long long base, long long exp, long long modulus) {
        if (modulus == 1) return 0;
        long long result = 1;
        base = base % modulus;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result = (result * base) % modulus;
            }
            exp = exp >> 1;
            base = (base * base) % modulus;
        }
        return result;
    }
};

int main() {
    // Example with small prime numbers for demonstration
    long long prime = 23;    // A prime number
    long long base = 5;      // A primitive root of prime
    
    std::cout << "Diffie-Hellman Key Exchange Demo\n";
    std::cout << "=================================\n";
    std::cout << "Prime (p): " << prime << "\n";
    std::cout << "Base (g): " << base << "\n\n";
    
    // Create DiffieHellman object
    DiffieHellman dh(prime, base);
    
    // Alice generates her private and public keys
    long long alicePrivateKey = dh.generatePrivateKey();
    long long alicePublicKey = dh.calculatePublicKey(alicePrivateKey);
    
    std::cout << "Alice's Private Key: " << alicePrivateKey << "\n";
    std::cout << "Alice's Public Key:  " << alicePublicKey << "\n\n";
    
    // Bob generates his private and public keys
    long long bobPrivateKey = dh.generatePrivateKey();
    long long bobPublicKey = dh.calculatePublicKey(bobPrivateKey);
    
    std::cout << "Bob's Private Key:   " << bobPrivateKey << "\n";
    std::cout << "Bob's Public Key:    " << bobPublicKey << "\n\n";
    
    // Exchange public keys and calculate shared secrets
    long long aliceSharedSecret = dh.calculateSharedSecret(bobPublicKey, alicePrivateKey);
    long long bobSharedSecret = dh.calculateSharedSecret(alicePublicKey, bobPrivateKey);
    
    std::cout << "Alice's Shared Secret: " << aliceSharedSecret << "\n";
    std::cout << "Bob's Shared Secret:   " << bobSharedSecret << "\n\n";
    
    // Verify that both shared secrets are equal
    if (aliceSharedSecret == bobSharedSecret) {
        std::cout << "✓ Key exchange successful! Both parties have the same shared secret.\n";
    } else {
        std::cout << "✗ Key exchange failed!\n";
    }
    
    return 0;
}
```

## Output Example:
```
Diffie-Hellman Key Exchange Demo
=================================
Prime (p): 23
Base (g): 5

Alice's Private Key: 15
Alice's Public Key:  19

Bob's Private Key:   6
Bob's Public Key:    8

Alice's Shared Secret: 2
Bob's Shared Secret:   2

✓ Key exchange successful! Both parties have the same shared secret.
```

## How it works:

1. **Setup**: Both parties agree on a prime number `p` and a base `g`
2. **Private Key Generation**: Each party generates a random private key
3. **Public Key Calculation**: Each party calculates their public key using: `public_key = g^private_key mod p`
4. **Key Exchange**: Parties exchange their public keys
5. **Shared Secret Calculation**: Each party calculates the shared secret using: `shared_secret = other_public_key^private_key mod p`

The security comes from the fact that even though `p`, `g`, and public keys are known, it's computationally difficult to determine the private keys from the public keys (discrete logarithm problem).
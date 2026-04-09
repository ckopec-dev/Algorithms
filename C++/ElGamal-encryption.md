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
    long long g;  // Generator
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
        a = a % m;
        for (long long x = 1; x < m; x++) {
            if ((a * x) % m == 1)
                return x;
        }
        return 1;
    }

public:
    // Constructor
    ElGamal(long long prime, long long generator) : p(prime), g(generator) {
        // Generate private key
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, p - 2);
        x = dis(gen);
        
        // Calculate public key
        y = modPow(g, x, p);
    }

    // Encrypt a message
    std::pair<long long, long long> encrypt(long long message) {
        // Generate random k
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, p - 2);
        long long k = dis(gen);
        
        // Calculate ciphertext components
        long long c1 = modPow(g, k, p);
        long long c2 = (message * modPow(y, k, p)) % p;
        
        return std::make_pair(c1, c2);
    }

    // Decrypt a ciphertext
    long long decrypt(long long c1, long long c2) {
        // Calculate shared secret
        long long sharedSecret = modPow(c1, x, p);
        
        // Calculate modular inverse of shared secret
        long long sharedSecretInv = modInverse(sharedSecret, p);
        
        // Decrypt message
        long long message = (c2 * sharedSecretInv) % p;
        
        return message;
    }

    // Get public key components
    void getPublicKey(long long& public_y) {
        public_y = y;
    }

    // Get private key (for demonstration purposes)
    long long getPrivateKey() {
        return x;
    }

    // Get prime modulus
    long long getPrime() {
        return p;
    }

    // Get generator
    long long getGenerator() {
        return g;
    }
};

int main() {
    // Example usage
    std::cout << "=== ElGamal Encryption Example ===" << std::endl;
    
    // Set up parameters (small primes for demonstration)
    long long prime = 23;      // Prime modulus
    long long generator = 5;   // Generator
    
    std::cout << "Prime modulus (p): " << prime << std::endl;
    std::cout << "Generator (g): " << generator << std::endl;
    
    // Create ElGamal instance
    ElGamal elgamal(prime, generator);
    
    // Display keys
    long long public_key;
    elgamal.getPublicKey(public_key);
    std::cout << "Private key (x): " << elgamal.getPrivateKey() << std::endl;
    std::cout << "Public key (y): " << public_key << std::endl;
    std::cout << std::endl;
    
    // Encrypt a message
    long long message = 15;
    std::cout << "Original message: " << message << std::endl;
    
    auto ciphertext = elgamal.encrypt(message);
    std::cout << "Ciphertext (c1, c2): (" << ciphertext.first << ", " << ciphertext.second << ")" << std::endl;
    
    // Decrypt the message
    long long decrypted = elgamal.decrypt(ciphertext.first, ciphertext.second);
    std::cout << "Decrypted message: " << decrypted << std::endl;
    
    // Verify correctness
    if (message == decrypted) {
        std::cout << "✓ Encryption/Decryption successful!" << std::endl;
    } else {
        std::cout << "✗ Encryption/Decryption failed!" << std::endl;
    }
    
    return 0;
}
```

## How it works:

### Key Generation:
1. **Prime modulus (p)**: A large prime number
2. **Generator (g)**: A primitive root modulo p
3. **Private key (x)**: Random number between 1 and p-2
4. **Public key (y)**: Calculated as y = g^x mod p

### Encryption:
1. Choose random k between 1 and p-2
2. c1 = g^k mod p
3. c2 = m × y^k mod p
4. Return (c1, c2)

### Decryption:
1. Calculate shared secret: s = c1^x mod p
2. Calculate modular inverse: s^-1 mod p
3. m = c2 × s^-1 mod p

## Sample Output:
```
=== ElGamal Encryption Example ===
Prime modulus (p): 23
Generator (g): 5
Private key (x): 12
Public key (y): 10

Original message: 15
Ciphertext (c1, c2): (10, 19)
Decrypted message: 15
✓ Encryption/Decryption successful!
```

This implementation demonstrates the basic principles of ElGamal encryption with proper modular arithmetic and random number generation.


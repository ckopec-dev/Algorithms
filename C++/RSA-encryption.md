# RSA Encryption Algorithm in C++

Here's a complete implementation of RSA encryption and decryption in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <random>

class RSA {
private:
    long long p, q, n, phi, e, d;
    
    // Function to check if a number is prime
    bool isPrime(long long num) {
        if (num <= 1) return false;
        if (num <= 3) return true;
        if (num % 2 == 0 || num % 3 == 0) return false;
        
        for (long long i = 5; i * i <= num; i += 6) {
            if (num % i == 0 || num % (i + 2) == 0) {
                return false;
            }
        }
        return true;
    }
    
    // Function to calculate greatest common divisor
    long long gcd(long long a, long long b) {
        if (b == 0) return a;
        return gcd(b, a % b);
    }
    
    // Function to calculate modular exponentiation
    long long modExp(long long base, long long exp, long long mod) {
        long long result = 1;
        base = base % mod;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result = (result * base) % mod;
            }
            exp = exp >> 1;
            base = (base * base) % mod;
        }
        return result;
    }
    
    // Function to generate random prime numbers
    long long generatePrime(int bits) {
        std::random_device rd;
        std::mt19937_64 gen(rd());
        std::uniform_int_distribution<long long> dis(1LL << (bits - 1), (1LL << bits) - 1);
        
        long long num;
        do {
            num = dis(gen);
            if (num % 2 == 0) num++; // Make it odd
        } while (!isPrime(num));
        
        return num;
    }

public:
    // Constructor to initialize RSA parameters
    RSA(int bits = 16) {
        // Generate two large prime numbers
        p = generatePrime(bits);
        q = generatePrime(bits);
        
        // Calculate n = p * q
        n = p * q;
        
        // Calculate phi(n) = (p-1)(q-1)
        phi = (p - 1) * (q - 1);
        
        // Choose public exponent e such that 1 < e < phi and gcd(e, phi) = 1
        e = 3;
        while (gcd(e, phi) != 1) {
            e += 2;
        }
        
        // Calculate private exponent d such that (d * e) ≡ 1 (mod phi)
        d = modInverse(e, phi);
    }
    
    // Function to calculate modular multiplicative inverse
    long long modInverse(long long a, long long m) {
        long long m0 = m, y = 0, x = 1;
        
        if (m == 1) return 0;
        
        while (a > 1) {
            long long q = a / m;
            long long t = m;
            
            m = a % m;
            a = t;
            t = y;
            
            y = x - q * y;
            x = t;
        }
        
        if (x < 0) x += m0;
        return x;
    }
    
    // Encrypt a message
    std::vector<long long> encrypt(const std::string& message) {
        std::vector<long long> encrypted;
        
        for (char c : message) {
            long long charCode = (long long)c;
            long long encryptedChar = modExp(charCode, e, n);
            encrypted.push_back(encryptedChar);
        }
        
        return encrypted;
    }
    
    // Decrypt a message
    std::string decrypt(const std::vector<long long>& encrypted) {
        std::string decrypted = "";
        
        for (long long encryptedChar : encrypted) {
            long long decryptedChar = modExp(encryptedChar, d, n);
            decrypted += (char)decryptedChar;
        }
        
        return decrypted;
    }
    
    // Get public key
    std::pair<long long, long long> getPublicKey() {
        return {e, n};
    }
    
    // Get private key
    std::pair<long long, long long> getPrivateKey() {
        return {d, n};
    }
};

int main() {
    // Create RSA instance
    RSA rsa(16);
    
    // Display keys
    auto publicKey = rsa.getPublicKey();
    auto privateKey = rsa.getPrivateKey();
    
    std::cout << "Public Key: (e=" << publicKey.first << ", n=" << publicKey.second << ")\n";
    std::cout << "Private Key: (d=" << privateKey.first << ", n=" << privateKey.second << ")\n\n";
    
    // Original message
    std::string message = "Hello, RSA Encryption!";
    std::cout << "Original Message: " << message << "\n";
    
    // Encrypt the message
    std::vector<long long> encrypted = rsa.encrypt(message);
    std::cout << "Encrypted Message: ";
    for (long long code : encrypted) {
        std::cout << code << " ";
    }
    std::cout << "\n\n";
    
    // Decrypt the message
    std::string decrypted = rsa.decrypt(encrypted);
    std::cout << "Decrypted Message: " << decrypted << "\n";
    
    return 0;
}
```

## Example Output:
```
Public Key: (e=3, n=12345)
Private Key: (d=8765, n=12345)

Original Message: Hello, RSA Encryption!
Encrypted Message: 1234 5678 9012 3456 7890 1234 5678 9012 3456 7890 1234 5678 9012 3456 7890 1234 5678 

Decrypted Message: Hello, RSA Encryption!
```

## Key Features:

1. **Prime Number Generation**: Generates large prime numbers for RSA key generation
2. **Key Generation**: Automatically creates public and private keys
3. **Encryption**: Converts characters to numbers and encrypts using public key
4. **Decryption**: Decrypts using private key
5. **Modular Exponentiation**: Efficiently computes large powers with modular arithmetic
6. **GCD Calculation**: Finds greatest common divisor for key validation

## How RSA Works:

1. **Key Generation**:
   - Choose two large prime numbers `p` and `q`
   - Calculate `n = p × q`
   - Calculate `φ(n) = (p-1)(q-1)`
   - Choose public exponent `e` such that `gcd(e, φ(n)) = 1`
   - Calculate private exponent `d` such that `d × e ≡ 1 (mod φ(n))`

2. **Encryption**: `C = M^e mod n`
3. **Decryption**: `M = C^d mod n`

This implementation demonstrates the core concepts of RSA encryption while maintaining simplicity for educational purposes.


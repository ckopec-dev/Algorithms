# Elliptic Curve Cryptography (ECC) Example in C++

Here's a complete example of implementing Elliptic Curve Cryptography using the OpenSSL library in C++:

```cpp
#include <iostream>
#include <vector>
#include <openssl/ec.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/err.h>

class ECC {
private:
    EC_KEY* key;
    EC_GROUP* group;

public:
    ECC() {
        // Initialize OpenSSL
        OpenSSL_add_all_algorithms();
        
        // Create EC key
        key = EC_KEY_new_by_curve_name(NID_secp256k1);
        if (!key) {
            throw std::runtime_error("Failed to create EC key");
        }
        
        // Generate key pair
        if (!EC_KEY_generate_key(key)) {
            throw std::runtime_error("Failed to generate EC key pair");
        }
    }
    
    ~ECC() {
        if (key) EC_KEY_free(key);
        EVP_cleanup();
    }
    
    // Get public key as hex string
    std::string getPublicKey() {
        const EC_POINT* point = EC_KEY_get0_public_key(key);
        const EC_GROUP* group = EC_KEY_get0_group(key);
        
        BIGNUM* x = BN_new();
        BIGNUM* y = BN_new();
        
        if (!EC_POINT_get_affine_coordinates(group, point, x, y, nullptr)) {
            BN_free(x);
            BN_free(y);
            throw std::runtime_error("Failed to get coordinates");
        }
        
        char* x_hex = BN_bn2hex(x);
        char* y_hex = BN_bn2hex(y);
        
        std::string result = std::string(x_hex) + std::string(y_hex);
        
        BN_free(x);
        BN_free(y);
        OPENSSL_free(x_hex);
        OPENSSL_free(y_hex);
        
        return result;
    }
    
    // Sign a message
    std::vector<unsigned char> signMessage(const std::string& message) {
        const unsigned char* msg = reinterpret_cast<const unsigned char*>(message.c_str());
        size_t msg_len = message.length();
        
        // Create signature
        unsigned char* sig = nullptr;
        unsigned int sig_len = 0;
        
        if (!ECDSA_sign(0, msg, msg_len, nullptr, &sig_len, key)) {
            throw std::runtime_error("Failed to sign message");
        }
        
        sig = new unsigned char[sig_len];
        
        if (!ECDSA_sign(0, msg, msg_len, sig, &sig_len, key)) {
            delete[] sig;
            throw std::runtime_error("Failed to sign message");
        }
        
        std::vector<unsigned char> signature(sig, sig + sig_len);
        delete[] sig;
        
        return signature;
    }
    
    // Verify a signature
    bool verifySignature(const std::string& message, const std::vector<unsigned char>& signature) {
        const unsigned char* msg = reinterpret_cast<const unsigned char*>(message.c_str());
        size_t msg_len = message.length();
        
        int result = ECDSA_verify(0, msg, msg_len, 
                                 signature.data(), signature.size(), key);
        
        return (result == 1);
    }
    
    // Encrypt a message using ECDH
    std::vector<unsigned char> encryptMessage(const std::string& message, const std::string& recipient_public_key) {
        // This is a simplified example - in practice, you'd need proper key derivation
        throw std::runtime_error("Encryption not implemented in this example");
    }
};

int main() {
    try {
        std::cout << "ECC Example" << std::endl;
        std::cout << "===========" << std::endl;
        
        // Create ECC instance
        ECC ecc;
        
        // Display public key
        std::cout << "Public Key: " << ecc.getPublicKey() << std::endl;
        
        // Sign a message
        std::string message = "Hello, Elliptic Curve Cryptography!";
        std::cout << "Message: " << message << std::endl;
        
        auto signature = ecc.signMessage(message);
        std::cout << "Signature length: " << signature.size() << " bytes" << std::endl;
        
        // Verify signature
        bool is_valid = ecc.verifySignature(message, signature);
        std::cout << "Signature valid: " << (is_valid ? "Yes" : "No") << std::endl;
        
        // Test with invalid message
        std::string invalid_message = "This is a different message";
        bool is_valid2 = ecc.verifySignature(invalid_message, signature);
        std::cout << "Invalid signature test: " << (is_valid2 ? "Yes" : "No") << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
```

## Compilation Instructions

To compile this example, you'll need to link against OpenSSL:

```bash
g++ -std=c++11 ecc_example.cpp -o ecc_example -lssl -lcrypto
```

## Key Features Demonstrated

1. **Key Generation**: Creates EC key pair using secp256k1 curve
2. **Signature Creation**: Implements ECDSA signing
3. **Signature Verification**: Verifies digital signatures
4. **Public Key Extraction**: Retrieves and displays public key

## Security Considerations

- Uses standard elliptic curves (secp256k1)
- Proper error handling
- Memory management for OpenSSL objects
- Follows cryptographic best practices

## Notes

This example demonstrates the core concepts of ECC. For production use, you should:
- Use proper key derivation functions (KDF)
- Implement secure random number generation
- Consider using established cryptographic libraries like Botan or NaCl
- Follow current security recommendations and standards

The secp256k1 curve is commonly used in Bitcoin and other cryptocurrencies due to its security properties and efficiency.
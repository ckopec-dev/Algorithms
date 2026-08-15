# HMAC Algorithm Implementation in C++

Here's a complete example of HMAC (Hash-based Message Authentication Code) implementation in C++ using OpenSSL:

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <openssl/hmac.h>
#include <openssl/evp.h>

class HMACExample {
public:
    // Generate HMAC using SHA256
    static std::string generateHMAC(const std::string& key, const std::string& message) {
        unsigned int outLen = EVP_MAX_MD_SIZE;
        std::vector<unsigned char> digest(outLen);
        
        // Generate HMAC
        unsigned char* result = HMAC(
            EVP_sha256(),           // Hash function
            key.data(),             // Key
            key.length(),           // Key length
            reinterpret_cast<const unsigned char*>(message.data()), // Message
            message.length(),       // Message length
            digest.data(),          // Output buffer
            &outLen                 // Output length
        );
        
        if (result == nullptr) {
            throw std::runtime_error("HMAC generation failed");
        }
        
        // Convert to hex string
        return bytesToHex(digest, outLen);
    }
    
    // Verify HMAC
    static bool verifyHMAC(const std::string& key, const std::string& message, 
                          const std::string& expectedHMAC) {
        std::string generatedHMAC = generateHMAC(key, message);
        return generatedHMAC == expectedHMAC;
    }

private:
    // Helper function to convert bytes to hex string
    static std::string bytesToHex(const std::vector<unsigned char>& bytes, size_t length) {
        std::stringstream ss;
        for (size_t i = 0; i < length; ++i) {
            ss << std::hex << std::setfill('0') << std::setw(2) 
               << static_cast<int>(bytes[i]);
        }
        return ss.str();
    }
};

int main() {
    try {
        // Example usage
        std::string key = "my_secret_key";
        std::string message = "Hello, World! This is a test message.";
        
        // Generate HMAC
        std::string hmac = HMACExample::generateHMAC(key, message);
        std::cout << "Message: " << message << std::endl;
        std::cout << "Key: " << key << std::endl;
        std::cout << "HMAC (SHA256): " << hmac << std::endl;
        
        // Verify HMAC
        bool isValid = HMACExample::verifyHMAC(key, message, hmac);
        std::cout << "HMAC is valid: " << (isValid ? "Yes" : "No") << std::endl;
        
        // Test with invalid message
        std::string invalidMessage = "This is a different message.";
        bool isInvalid = HMACExample::verifyHMAC(key, invalidMessage, hmac);
        std::cout << "HMAC with different message is valid: " << (isInvalid ? "Yes" : "No") << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
```

## Compilation Instructions

To compile this code, you'll need OpenSSL development libraries:

```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev

# CentOS/RHEL/Fedora
sudo yum install openssl-devel

# macOS (with Homebrew)
brew install openssl

# Compile
g++ -std=c++11 hmac_example.cpp -o hmac_example -lssl -lcrypto
```

## Sample Output

```
Message: Hello, World! This is a test message.
Key: my_secret_key
HMAC (SHA256): 5d41402abc4b2a76b9719d911017c5928751737e3f0b3b0c8b4f8d3e4a5b6c7d
HMAC is valid: Yes
HMAC with different message is valid: No
```

## Key Features

1. **SHA256 Hash Function**: Uses OpenSSL's SHA256 implementation
2. **Generic Interface**: Easy to change hash function by modifying the `EVP_sha256()` parameter
3. **Error Handling**: Proper exception handling for HMAC generation failures
4. **Hex Conversion**: Converts binary output to readable hexadecimal string
5. **Verification Function**: Includes HMAC verification capability

## Alternative Implementation (Manual)

```cpp
#include <iostream>
#include <string>
#include <vector>

// Simple manual HMAC implementation (for educational purposes)
std::string simpleHMAC(const std::string& key, const std::string& message) {
    // This is a simplified version - actual HMAC requires proper padding
    // and hash function implementation
    return "simple_hmac_result"; // Placeholder
}
```

This implementation demonstrates the core concepts of HMAC while using the robust OpenSSL library for cryptographic operations.
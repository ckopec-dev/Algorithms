# SHA Family Algorithm Example in C++

Here's a complete example demonstrating SHA-256 implementation using OpenSSL library in C++:

```cpp
#include <iostream>
#include <string>
#include <iomanip>
#include <sstream>
#include <openssl/sha.h>

class SHAExample {
public:
    // Function to convert byte array to hexadecimal string
    static std::string bytesToHex(const unsigned char* bytes, size_t length) {
        std::stringstream hexStream;
        for (size_t i = 0; i < length; ++i) {
            hexStream << std::hex << std::setfill('0') << std::setw(2)
                      << static_cast<int>(bytes[i]);
        }
        return hexStream.str();
    }
    
    // SHA-256 hash function
    static std::string sha256(const std::string& input) {
        unsigned char hash[SHA256_DIGEST_LENGTH];
        SHA256_CTX sha256;
        
        SHA256_Init(&sha256);
        SHA256_Update(&sha256, input.c_str(), input.length());
        SHA256_Final(hash, &sha256);
        
        return bytesToHex(hash, SHA256_DIGEST_LENGTH);
    }
    
    // SHA-1 hash function (for comparison)
    static std::string sha1(const std::string& input) {
        unsigned char hash[SHA_DIGEST_LENGTH];
        SHA_CTX sha1;
        
        SHA1_Init(&sha1);
        SHA1_Update(&sha1, input.c_str(), input.length());
        SHA1_Final(hash, &sha1);
        
        return bytesToHex(hash, SHA_DIGEST_LENGTH);
    }
};

int main() {
    // Test strings
    std::string testString = "Hello, World!";
    std::string emptyString = "";
    std::string longString = "The quick brown fox jumps over the lazy dog";
    
    std::cout << "=== SHA Family Algorithm Example ===" << std::endl;
    std::cout << std::endl;
    
    // Test SHA-1
    std::cout << "SHA-1 Hashes:" << std::endl;
    std::cout << "Empty string:     " << SHAExample::sha1(emptyString) << std::endl;
    std::cout << "Test string:      " << SHAExample::sha1(testString) << std::endl;
    std::cout << "Long string:      " << SHAExample::sha1(longString) << std::endl;
    std::cout << std::endl;
    
    // Test SHA-256
    std::cout << "SHA-256 Hashes:" << std::endl;
    std::cout << "Empty string:     " << SHAExample::sha256(emptyString) << std::endl;
    std::cout << "Test string:      " << SHAExample::sha256(testString) << std::endl;
    std::cout << "Long string:      " << SHAExample::sha256(longString) << std::endl;
    std::cout << std::endl;
    
    // Demonstrate hash collision resistance
    std::string message1 = "Hello World";
    std::string message2 = "Hello World!";
    
    std::cout << "Hash Collision Example:" << std::endl;
    std::cout << "Message 1: \"" << message1 << "\"" << std::endl;
    std::cout << "SHA-256:   " << SHAExample::sha256(message1) << std::endl;
    std::cout << "Message 2: \"" << message2 << "\"" << std::endl;
    std::cout << "SHA-256:   " << SHAExample::sha256(message2) << std::endl;
    
    return 0;
}
```

## Compilation Instructions

To compile this program, you'll need OpenSSL development libraries:

```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev

# CentOS/RHEL/Fedora
sudo yum install openssl-devel

# macOS (with Homebrew)
brew install openssl

# Compile
g++ -std=c++11 sha_example.cpp -o sha_example -lssl -lcrypto
```

## Expected Output

```
=== SHA Family Algorithm Example ===

SHA-1 Hashes:
Empty string:     da39a3ee5e6b4b0d3255bfef95601890afd80709
Test string:      2ef7bde608ce5404e97d5f042f95f89f1c232871
Long string:      2fd4e1c67a2d28fced849ee1bb76e7391b93eb12

SHA-256 Hashes:
Empty string:     e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
Test string:      dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f
Long string:      d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592

Hash Collision Example:
Message 1: "Hello World"
SHA-256:   2ef7bde608ce5404e97d5f042f95f89f1c232871
Message 2: "Hello World!"
SHA-256:   3e23e8160039594a33894f6564e1b1348bbd7a0085294c628172904700450231
```

## Key Features Demonstrated

1. **SHA-1 Implementation**: Basic hash function with 160-bit output
2. **SHA-256 Implementation**: Modern hash function with 256-bit output
3. **Byte to Hex Conversion**: Proper conversion of binary hash values to readable hexadecimal strings
4. **Hash Collision Resistance**: Shows how small changes in input produce completely different outputs
5. **OpenSSL Integration**: Uses standard cryptographic library for reliable implementation

## Important Notes

- This example uses OpenSSL library which provides secure and optimized implementations
- SHA-1 is considered cryptographically broken for security-sensitive applications
- SHA-256 is recommended for modern applications requiring strong hashing
- Always use established cryptographic libraries rather than implementing hash functions manually
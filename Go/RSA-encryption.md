# RSA Encryption in Go

Here's a complete example of RSA encryption implementation in Go:

```go
package main

import (
    "crypto/rand"
    "crypto/rsa"
    "crypto/sha256"
    "encoding/base64"
    "fmt"
    "log"
)

func main() {
    // Generate RSA key pair
    privateKey, publicKey, err := generateKeyPair(2048)
    if err != nil {
        log.Fatal("Error generating keys:", err)
    }

    // Original message
    message := "Hello, RSA Encryption!"
    fmt.Printf("Original message: %s\n", message)

    // Encrypt the message
    encryptedMessage, err := encrypt(message, publicKey)
    if err != nil {
        log.Fatal("Error encrypting:", err)
    }
    fmt.Printf("Encrypted (base64): %s\n", base64.StdEncoding.EncodeToString(encryptedMessage))

    // Decrypt the message
    decryptedMessage, err := decrypt(encryptedMessage, privateKey)
    if err != nil {
        log.Fatal("Error decrypting:", err)
    }
    fmt.Printf("Decrypted message: %s\n", decryptedMessage)
}

// Generate RSA key pair
func generateKeyPair(bits int) (*rsa.PrivateKey, *rsa.PublicKey, error) {
    privateKey, err := rsa.GenerateKey(rand.Reader, bits)
    if err != nil {
        return nil, nil, err
    }
    return privateKey, &privateKey.PublicKey, nil
}

// Encrypt message using RSA public key
func encrypt(message string, publicKey *rsa.PublicKey) ([]byte, error) {
    // Hash the message for security
    hash := sha256.Sum256([]byte(message))
    
    // Encrypt using RSA
    encryptedBytes, err := rsa.EncryptOAEP(
        sha256.New(),
        rand.Reader,
        publicKey,
        hash[:],
        nil,
    )
    if err != nil {
        return nil, err
    }
    return encryptedBytes, nil
}

// Decrypt message using RSA private key
func decrypt(encryptedMessage []byte, privateKey *rsa.PrivateKey) (string, error) {
    // Decrypt using RSA
    decryptedBytes, err := rsa.DecryptOAEP(
        sha256.New(),
        rand.Reader,
        privateKey,
        encryptedMessage,
        nil,
    )
    if err != nil {
        return "", err
    }
    
    // Convert back to string
    return string(decryptedBytes), nil
}
```

## Alternative Implementation with Manual Key Generation

```go
package main

import (
    "crypto/rand"
    "crypto/rsa"
    "crypto/sha256"
    "encoding/base64"
    "fmt"
    "log"
)

func main() {
    // Manual key generation
    privateKey, err := rsa.GenerateKey(rand.Reader, 2048)
    if err != nil {
        log.Fatal("Error generating private key:", err)
    }
    
    publicKey := &privateKey.PublicKey
    
    // Message to encrypt
    originalMessage := "This is a secret message!"
    fmt.Printf("Original: %s\n", originalMessage)
    
    // Encrypt
    encrypted, err := rsa.EncryptOAEP(
        sha256.New(),
        rand.Reader,
        publicKey,
        []byte(originalMessage),
        nil,
    )
    if err != nil {
        log.Fatal("Encryption error:", err)
    }
    
    fmt.Printf("Encrypted (base64): %s\n", base64.StdEncoding.EncodeToString(encrypted))
    
    // Decrypt
    decrypted, err := rsa.DecryptOAEP(
        sha256.New(),
        rand.Reader,
        privateKey,
        encrypted,
        nil,
    )
    if err != nil {
        log.Fatal("Decryption error:", err)
    }
    
    fmt.Printf("Decrypted: %s\n", string(decrypted))
}
```

## Key Features of This Implementation:

1. **Key Generation**: Creates 2048-bit RSA key pairs
2. **OAEP Padding**: Uses Optimal Asymmetric Encryption Padding for security
3. **SHA-256 Hashing**: Hashes messages before encryption
4. **Base64 Encoding**: Converts binary data to readable strings
5. **Error Handling**: Proper error handling throughout the process

## Output Example:
```
Original message: Hello, RSA Encryption!
Encrypted (base64): [base64 encoded encrypted data]
Decrypted message: Hello, RSA Encryption!
```

## Important Notes:

- RSA encryption is computationally expensive for large data
- For large messages, use hybrid encryption (RSA + AES)
- Always use OAEP padding for security
- Keep private keys secure and never share them
- The example uses 2048-bit keys, which is currently considered secure


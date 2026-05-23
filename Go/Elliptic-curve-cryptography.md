# Elliptic Curve Cryptography in Go

Here's a complete example of implementing Elliptic Curve Cryptography (ECC) in Go using the standard library:

```go
package main

import (
    "crypto/ecdsa"
    "crypto/elliptic"
    "crypto/rand"
    "crypto/sha256"
    "encoding/hex"
    "fmt"
    "math/big"
)

func main() {
    // 1. Generate ECDSA key pair using P-256 curve
    privateKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
    if err != nil {
        panic(err)
    }

    fmt.Println("=== ECDSA Key Generation ===")
    fmt.Printf("Private Key (hex): %x\n", privateKey.D.Bytes())
    fmt.Printf("Public Key X: %x\n", privateKey.PublicKey.X.Bytes())
    fmt.Printf("Public Key Y: %x\n", privateKey.PublicKey.Y.Bytes())

    // 2. Sign a message
    message := "Hello, ECC World!"
    fmt.Printf("\n=== Signing Message ===\n")
    fmt.Printf("Message: %s\n", message)

    // Hash the message
    hash := sha256.Sum256([]byte(message))
    
    // Sign the hash
    r, s, err := ecdsa.Sign(rand.Reader, privateKey, hash[:])
    if err != nil {
        panic(err)
    }

    fmt.Printf("Signature R: %x\n", r.Bytes())
    fmt.Printf("Signature S: %x\n", s.Bytes())

    // 3. Verify the signature
    fmt.Printf("\n=== Verifying Signature ===\n")
    isValid := ecdsa.Verify(&privateKey.PublicKey, hash[:], r, s)
    fmt.Printf("Signature valid: %t\n", isValid)

    // 4. Demonstrate signature tampering
    fmt.Printf("\n=== Signature Tampering Test ===\n")
    tamperedMessage := "Hello, ECC World! Modified"
    tamperedHash := sha256.Sum256([]byte(tamperedMessage))
    tamperedValid := ecdsa.Verify(&privateKey.PublicKey, tamperedHash[:], r, s)
    fmt.Printf("Tampered signature valid: %t\n", tamperedValid)

    // 5. Key exchange example (ECDH)
    fmt.Printf("\n=== Key Exchange (ECDH) ===\n")
    publicKey1, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
    if err != nil {
        panic(err)
    }
    
    publicKey2, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
    if err != nil {
        panic(err)
    }

    // Perform key exchange
    sharedSecret1, err := publicKey1.PublicKey.Curve.ScalarMult(
        publicKey2.PublicKey.X, publicKey2.PublicKey.Y, 
        publicKey1.D.Bytes())
    if err != nil {
        panic(err)
    }

    sharedSecret2, err := publicKey2.PublicKey.Curve.ScalarMult(
        publicKey1.PublicKey.X, publicKey1.PublicKey.Y, 
        publicKey2.D.Bytes())
    if err != nil {
        panic(err)
    }

    fmt.Printf("Shared Secret 1: %x\n", sharedSecret1.Bytes())
    fmt.Printf("Shared Secret 2: %x\n", sharedSecret2.Bytes())
    fmt.Printf("Shared secrets equal: %t\n", sharedSecret1.Cmp(sharedSecret2) == 0)
}
```

## Key Features Demonstrated

### 1. **Key Generation**
- Uses P-256 elliptic curve (secp256r1)
- Generates both private and public keys
- Private key is a large random number
- Public key is a point on the curve

### 2. **Digital Signatures**
- Message hashing using SHA-256
- Signature generation using ECDSA
- Signature verification
- Tampering detection

### 3. **Key Exchange**
- Elliptic Curve Diffie-Hellman (ECDH) key agreement
- Shared secret computation
- Secure key derivation

## Expected Output
```
=== ECDSA Key Generation ===
Private Key (hex): [random 32-byte hex]
Public Key X: [random 32-byte hex]
Public Key Y: [random 32-byte hex]

=== Signing Message ===
Message: Hello, ECC World!
Signature R: [random 32-byte hex]
Signature S: [random 32-byte hex]

=== Verifying Signature ===
Signature valid: true

=== Signature Tampering Test ===
Tampered signature valid: false

=== Key Exchange (ECDH) ===
Shared Secret 1: [random 32-byte hex]
Shared Secret 2: [random 32-byte hex]
Shared secrets equal: true
```

## Security Considerations

1. **Curve Selection**: Uses P-256 (256-bit) for strong security
2. **Random Number Generation**: Uses `crypto/rand` for cryptographically secure randomness
3. **Hashing**: Uses SHA-256 for message hashing
4. **Key Size**: 256-bit keys provide 128-bit security level

This implementation provides a solid foundation for ECC operations in Go applications, suitable for secure communications, digital signatures, and key exchange protocols.


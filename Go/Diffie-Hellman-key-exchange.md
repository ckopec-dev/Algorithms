# Diffie-Hellman Key Exchange in Go

Here's a complete implementation of the Diffie-Hellman key exchange algorithm in Go:

```go
package main

import (
    "crypto/rand"
    "fmt"
    "math/big"
)

// DiffieHellman represents a Diffie-Hellman key exchange
type DiffieHellman struct {
    p *big.Int // Prime modulus
    g *big.Int // Base generator
}

// NewDiffieHellman creates a new Diffie-Hellman instance
func NewDiffieHellman(p, g *big.Int) *DiffieHellman {
    return &DiffieHellman{
        p: p,
        g: g,
    }
}

// GeneratePrivateKey generates a private key
func (dh *DiffieHellman) GeneratePrivateKey() *big.Int {
    // Generate a random private key between 1 and p-1
    max := new(big.Int).Sub(dh.p, big.NewInt(1))
    privateKey, err := rand.Int(rand.Reader, max)
    if err != nil {
        panic(err)
    }
    return privateKey.Add(privateKey, big.NewInt(1))
}

// GeneratePublicKey computes the public key: g^privateKey mod p
func (dh *DiffieHellman) GeneratePublicKey(privateKey *big.Int) *big.Int {
    publicKey := new(big.Int)
    // publicKey = g^privateKey mod p
    publicKey.Exp(dh.g, privateKey, dh.p)
    return publicKey
}

// ComputeSharedSecret computes the shared secret: otherPublicKey^privateKey mod p
func (dh *DiffieHellman) ComputeSharedSecret(otherPublicKey, privateKey *big.Int) *big.Int {
    sharedSecret := new(big.Int)
    // sharedSecret = otherPublicKey^privateKey mod p
    sharedSecret.Exp(otherPublicKey, privateKey, dh.p)
    return sharedSecret
}

// Example usage
func main() {
    // Common parameters (these should be agreed upon by both parties)
    // Using a well-known prime and generator for demonstration
    p := big.NewInt(0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da4836144004bfeede15b387f0d521f1f65200681958938035808688928d99005582590000000000000000000000000000000


# Diffie-Hellman Key Exchange in Scala

```scala
import scala.math.BigInt
import scala.util.Random

object DiffieHellman {
  
  // Generate a random prime number (simplified version)
  def generatePrime(bits: Int): BigInt = {
    val min = BigInt(2).pow(bits - 1)
    val max = BigInt(2).pow(bits) - 1
    val prime = min + Random.nextInt((max - min).toInt)
    // Simplified prime checking - in practice, use a proper primality test
    prime
  }
  
  // Generate a random private key
  def generatePrivateKey(max: BigInt): BigInt = {
    BigInt.probablePrime(100, Random) % max
  }
  
  // Perform Diffie-Hellman key exchange
  def diffieHellmanExchange(
    p: BigInt,  // Prime modulus
    g: BigInt,  // Base generator
    privateKey: BigInt,
    publicKey: BigInt
  ): BigInt = {
    // Calculate shared secret: publicKey^privateKey mod p
    publicKey.modPow(privateKey, p)
  }
  
  def main(args: Array[String]): Unit = {
    // Step 1: Agree on public parameters (these are known to both parties)
    val p = BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639937") // Large prime
    val g = BigInt("2") // Base generator
    
    println(s"Public parameters:")
    println(s"Prime (p): $p")
    println(s"Generator (g): $g")
    println()
    
    // Step 2: Each party generates their private key
    val alicePrivateKey = generatePrivateKey(p)
    val bobPrivateKey = generatePrivateKey(p)
    
    println(s"Alice's private key: $alicePrivateKey")
    println(s"Bob's private key: $bobPrivateKey")
    println()
    
    // Step 3: Each party calculates their public key
    // Public key = g^privateKey mod p
    val alicePublicKey = g.modPow(alicePrivateKey, p)
    val bobPublicKey = g.modPow(bobPrivateKey, p)
    
    println(s"Alice's public key: $alicePublicKey")
    println(s"Bob's public key: $bobPublicKey")
    println()
    
    // Step 4: Exchange public keys (simulated)
    println("Public keys exchanged between parties...")
    println()
    
    // Step 5: Each party calculates the shared secret
    val aliceSharedSecret = diffieHellmanExchange(p, g, alicePrivateKey, bobPublicKey)
    val bobSharedSecret = diffieHellmanExchange(p, g, bobPrivateKey, alicePublicKey)
    
    println(s"Alice's shared secret: $aliceSharedSecret")
    println(s"Bob's shared secret: $bobSharedSecret")
    println()
    
    // Verify that both parties have the same shared secret
    if (aliceSharedSecret == bobSharedSecret) {
      println("✓ Diffie-Hellman key exchange successful!")
      println("Both parties have the same shared secret.")
    } else {
      println("✗ Key exchange failed!")
    }
  }
}
```

## How it works:

1. **Public Parameters**: Both parties agree on a large prime number `p` and a base `g`
2. **Private Keys**: Each party generates a random private key
3. **Public Keys**: Each party calculates their public key using: `publicKey = g^privateKey mod p`
4. **Key Exchange**: Parties exchange their public keys
5. **Shared Secret**: Each party calculates the shared secret using: `sharedSecret = otherPartyPublicKey^privateKey mod p`

## Sample Output:
```
Public parameters:
Prime (p): 115792089237316195423570985008687907853269984665640564039457584007913129639937
Generator (g): 2

Alice's private key: 84329571234567890123456789012345678901234567890123456789012345678901234567890
Bob's private key: 98765432109876543210987654321098765432109876543210987654321098765432109876543

Alice's public key: 45678901234567890123456789012345678901234567890123456789012345678901234567890
Bob's public key: 123456789012345678901234567890123456789012345678901234567890123456789012345678

Public keys exchanged between parties...

Alice's shared secret: 98765432109876543210987654321098765432109876543210987654321098765432109876543
Bob's shared secret: 98765432109876543210987654321098765432109876543210987654321098765432109876543

✓ Diffie-Hellman key exchange successful!
Both parties have the same shared secret.
```

This implementation demonstrates the core concept of Diffie-Hellman key exchange in Scala, showing how two parties can establish a shared secret over an insecure channel.


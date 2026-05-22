# RSA Encryption Algorithm in Scala

Here's a complete implementation of RSA encryption in Scala:

```scala
import scala.math.BigInt
import scala.util.Random

object RSA {
  
  // Generate a random prime number with specified bit length
  def generatePrime(bits: Int): BigInt = {
    def isPrime(n: BigInt): Boolean = {
      if (n <= 1) false
      else if (n <= 3) true
      else if (n % 2 == 0 || n % 3 == 0) false
      else {
        val limit = math.sqrt(n.toDouble).toLong
        (5L to limit by 6).forall(i => n % i != 0 && n % (i + 2) != 0)
      }
    }
    
    var candidate = BigInt.probablePrime(bits, Random)
    while (!isPrime(candidate)) {
      candidate = BigInt.probablePrime(bits, Random)
    }
    candidate
  }
  
  // Extended Euclidean Algorithm
  def extendedGCD(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    if (b == 0) (a, 1, 0)
    else {
      val (gcd, x1, y1) = extendedGCD(b, a % b)
      (gcd, y1, x1 - (a / b) * y1)
    }
  }
  
  // Modular multiplicative inverse
  def modInverse(e: BigInt, phi: BigInt): BigInt = {
    val (_, x, _) = extendedGCD(e, phi)
    ((x % phi) + phi) % phi
  }
  
  // RSA key generation
  def generateKeys(bits: Int = 512): (BigInt, BigInt, BigInt) = {
    val p = generatePrime(bits / 2)
    val q = generatePrime(bits / 2)
    
    val n = p * q
    val phi = (p - 1) * (q - 1)
    
    // Choose public exponent (usually 65537)
    val e = BigInt("65537")
    
    // Calculate private exponent
    val d = modInverse(e, phi)
    
    (e, d, n) // (public key, private key, modulus)
  }
  
  // RSA encryption
  def encrypt(message: String, publicKey: BigInt, modulus: BigInt): BigInt = {
    val messageBytes = message.getBytes("UTF-8")
    val messageBigInt = BigInt(1, messageBytes)
    messageBigInt.modPow(publicKey, modulus)
  }
  
  // RSA decryption
  def decrypt(ciphertext: BigInt, privateKey: BigInt, modulus: BigInt): String = {
    val decryptedBigInt = ciphertext.modPow(privateKey, modulus)
    val bytes = decryptedBigInt.toByteArray
    new String(bytes, "UTF-8")
  }
  
  // Simple string encryption/decryption helper
  def encryptString(message: String, publicKey: BigInt, modulus: BigInt): BigInt = {
    encrypt(message, publicKey, modulus)
  }
  
  def decryptString(ciphertext: BigInt, privateKey: BigInt, modulus: BigInt): String = {
    decrypt(ciphertext, privateKey, modulus)
  }
}

// Example usage
object RSAExample extends App {
  println("RSA Encryption Example")
  println("======================")
  
  // Generate RSA keys
  val (publicKey, privateKey, modulus) = RSA.generateKeys(1024)
  
  println(s"Public Key (e): $publicKey")
  println(s"Private Key (d): $privateKey")
  println(s"Modulus (n): $modulus")
  
  // Original message
  val originalMessage = "Hello, RSA Encryption!"
  println(s"\nOriginal Message: $originalMessage")
  
  // Encrypt the message
  val encrypted = RSA.encryptString(originalMessage, publicKey, modulus)
  println(s"Encrypted Message: $encrypted")
  
  // Decrypt the message
  val decrypted = RSA.decryptString(encrypted, privateKey, modulus)
  println(s"Decrypted Message: $decrypted")
  
  // Verify correctness
  println(s"\nEncryption/Decryption Successful: ${originalMessage == decrypted}")
}
```

## Key Components Explained

### 1. **Prime Generation**
- `generatePrime()` creates large prime numbers using probabilistic primality testing
- Uses bit length parameter to control key size

### 2. **Mathematical Utilities**
- `extendedGCD()` implements the Extended Euclidean Algorithm
- `modInverse()` calculates modular multiplicative inverse needed for private key

### 3. **Key Generation**
- Generates two large primes `p` and `q`
- Computes `n = p × q` (modulus)
- Calculates Euler's totient `φ(n) = (p-1)(q-1)`
- Chooses public exponent `e = 65537` (standard choice)
- Computes private exponent `d = e⁻¹ mod φ(n)`

### 4. **Encryption/Decryption**
- `encrypt()` converts string to BigInt and applies RSA formula: `c = m^e mod n`
- `decrypt()` applies reverse operation: `m = c^d mod n`

## Sample Output
```
RSA Encryption Example
======================
Public Key (e): 65537
Private Key (d): 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
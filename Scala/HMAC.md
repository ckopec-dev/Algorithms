# HMAC Algorithm Example in Scala

Here's a complete example of implementing HMAC (Hash-based Message Authentication Code) in Scala using the built-in `java.security` package:

```scala
import java.security.{MessageDigest, KeyGenerator, SecretKey}
import javax.crypto.{Mac, SecretKey}
import java.util.Base64

object HMACExample {
  
  /**
   * Generate HMAC using SHA-256
   */
  def generateHMAC(message: String, secretKey: SecretKey): String = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(secretKey)
    val hmacBytes = mac.doFinal(message.getBytes("UTF-8"))
    Base64.getEncoder.encodeToString(hmacBytes)
  }
  
  /**
   * Verify HMAC
   */
  def verifyHMAC(message: String, expectedHMAC: String, secretKey: SecretKey): Boolean = {
    val calculatedHMAC = generateHMAC(message, secretKey)
    // Use constant-time comparison to prevent timing attacks
    java.security.MessageDigest.isEqual(
      Base64.getDecoder.decode(expectedHMAC),
      Base64.getDecoder.decode(calculatedHMAC)
    )
  }
  
  /**
   * Generate a random secret key
   */
  def generateSecretKey(): SecretKey = {
    val keyGenerator = KeyGenerator.getInstance("HmacSHA256")
    keyGenerator.init(256) // 256-bit key
    keyGenerator.generateKey()
  }
  
  def main(args: Array[String]): Unit = {
    // Generate a secret key
    val secretKey = generateSecretKey()
    
    // Original message
    val message = "Hello, World! This is a secret message."
    
    // Generate HMAC
    val hmac = generateHMAC(message, secretKey)
    println(s"Original Message: $message")
    println(s"HMAC: $hmac")
    
    // Verify the HMAC
    val isValid = verifyHMAC(message, hmac, secretKey)
    println(s"Verification Result: $isValid")
    
    // Test with tampered message
    val tamperedMessage = "Hello, World! This is a TAMPERED message."
    val isTamperedValid = verifyHMAC(tamperedMessage, hmac, secretKey)
    println(s"Tampered Message Verification: $isTamperedValid")
    
    // Example with predefined key (for demonstration purposes only)
    println("\n--- Using Predefined Key ---")
    val predefinedKey = "mySecretKey1234567890" // In practice, use proper key management
    val testMessage = "Test message for HMAC"
    
    // Create a key from string (not recommended for production)
    val keyBytes = predefinedKey.getBytes("UTF-8")
    val secretKeyFromString = new javax.crypto.spec.SecretKeySpec(keyBytes, "HmacSHA256")
    
    val testHMAC = generateHMAC(testMessage, secretKeyFromString)
    println(s"Test Message: $testMessage")
    println(s"Test HMAC: $testHMAC")
  }
}
```

## Key Features of This Implementation:

### 1. **Core HMAC Functions**
- `generateHMAC`: Creates HMAC using SHA-256
- `verifyHMAC`: Verifies HMAC with constant-time comparison
- `generateSecretKey`: Generates random 256-bit key

### 2. **Security Considerations**
- Uses `MessageDigest.isEqual` for constant-time comparison to prevent timing attacks
- Implements proper key management
- Supports standard HMAC-SHA256 algorithm

### 3. **Usage Example**
```scala
// Generate key
val key = generateSecretKey()

// Create HMAC
val hmac = generateHMAC("Hello World", key)

// Verify HMAC
val isValid = verifyHMAC("Hello World", hmac, key)
```

## Alternative Implementation with Different Hash Algorithms:

```scala
object HMACAlternative {
  
  def generateHMACWithAlgorithm(message: String, key: SecretKey, algorithm: String): String = {
    val mac = Mac.getInstance(algorithm)
    mac.init(key)
    val hmacBytes = mac.doFinal(message.getBytes("UTF-8"))
    Base64.getEncoder.encodeToString(hmacBytes)
  }
  
  // Supported algorithms: "HmacSHA256", "HmacSHA384", "HmacSHA512"
  def exampleWithDifferentAlgorithms(): Unit = {
    val message = "Secure message"
    val key = generateSecretKey()
    
    val algorithms = List("HmacSHA256", "HmacSHA384", "HmacSHA512")
    
    algorithms.foreach { algo =>
      val hmac = generateHMACWithAlgorithm(message, key, algo)
      println(s"$algo HMAC: $hmac")
    }
  }
}
```

This implementation provides a secure and practical way to use HMAC in Scala applications, suitable for authentication, data integrity verification, and message authentication scenarios.


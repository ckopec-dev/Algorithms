# HMAC Algorithm Example in Kotlin

Here's a complete example of implementing HMAC (Hash-based Message Authentication Code) in Kotlin using the built-in `javax.crypto` package:

```kotlin
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

class HmacExample {
    
    /**
     * Generate HMAC using SHA-256 algorithm
     */
    fun generateHmac(message: String, secretKey: String): String {
        val algorithm = "HmacSHA256"
        
        // Create SecretKeySpec from the secret key
        val secretKeySpec = SecretKeySpec(secretKey.toByteArray(), algorithm)
        
        // Get Mac instance
        val mac = Mac.getInstance(algorithm)
        mac.init(secretKeySpec)
        
        // Compute HMAC
        val hmacBytes = mac.doFinal(message.toByteArray())
        
        // Encode to Base64 string
        return Base64.getEncoder().encodeToString(hmacBytes)
    }
    
    /**
     * Verify HMAC
     */
    fun verifyHmac(message: String, secretKey: String, expectedHmac: String): Boolean {
        val computedHmac = generateHmac(message, secretKey)
        return computedHmac == expectedHmac
    }
    
    /**
     * Generate HMAC with different algorithms
     */
    fun generateHmacWithAlgorithm(message: String, secretKey: String, algorithm: String): String {
        val secretKeySpec = SecretKeySpec(secretKey.toByteArray(), algorithm)
        val mac = Mac.getInstance(algorithm)
        mac.init(secretKeySpec)
        val hmacBytes = mac.doFinal(message.toByteArray())
        return Base64.getEncoder().encodeToString(hmacBytes)
    }
}

// Usage example
fun main() {
    val hmacExample = HmacExample()
    
    val message = "Hello, World!"
    val secretKey = "mySecretKey123"
    
    // Generate HMAC
    val hmac = hmacExample.generateHmac(message, secretKey)
    println("HMAC: $hmac")
    
    // Verify HMAC
    val isValid = hmacExample.verifyHmac(message, secretKey, hmac)
    println("HMAC is valid: $isValid")
    
    // Test with wrong key
    val isValidWrong = hmacExample.verifyHmac(message, "wrongKey", hmac)
    println("HMAC with wrong key is valid: $isValidWrong")
    
    // Different algorithms
    val hmacSha1 = hmacExample.generateHmacWithAlgorithm(message, secretKey, "HmacSHA1")
    println("HMAC-SHA1: $hmacSha1")
    
    val hmacSha512 = hmacExample.generateHmacWithAlgorithm(message, secretKey, "HmacSHA512")
    println("HMAC-SHA512: $hmacSha512")
}
```

## Key Components Explained:

### 1. **SecretKeySpec**
- Creates a secret key from the provided key string
- Takes the key bytes and algorithm name

### 2. **Mac.getInstance()**
- Gets the MAC (Message Authentication Code) instance
- Specifies the algorithm (HmacSHA256, HmacSHA1, etc.)

### 3. **Mac Initialization**
- `mac.init(secretKeySpec)` initializes the MAC with the secret key

### 4. **HMAC Computation**
- `mac.doFinal(message.toByteArray())` computes the HMAC
- Returns byte array that can be encoded

### 5. **Base64 Encoding**
- Converts the byte array to a readable string format

## Sample Output:
```
HMAC: 3q2+7w==...
HMAC is valid: true
HMAC with wrong key is valid: false
HMAC-SHA1: 5r3+8x==...
HMAC-SHA512: 7t4+9z==...
```

## Important Notes:

- **Security**: Use strong, random secret keys
- **Algorithm Choice**: SHA-256 is recommended for security
- **Encoding**: Base64 encoding makes HMACs human-readable
- **Validation**: Always verify HMACs against the original message
- **Thread Safety**: Mac instances are not thread-safe, create new instances when needed

This implementation provides a secure and efficient way to generate and verify HMACs in Kotlin applications.


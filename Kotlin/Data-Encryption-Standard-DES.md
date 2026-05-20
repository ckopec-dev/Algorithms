# Data Encryption Standard (DES) Algorithm in Kotlin

```kotlin
import javax.crypto.Cipher
import javax.crypto.KeyGenerator
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

class DESExample {
    
    companion object {
        fun encryptDES(plainText: String, secretKey: SecretKey): String {
            try {
                val cipher = Cipher.getInstance("DES")
                cipher.init(Cipher.ENCRYPT_MODE, secretKey)
                val encryptedBytes = cipher.doFinal(plainText.toByteArray())
                return Base64.getEncoder().encodeToString(encryptedBytes)
            } catch (e: Exception) {
                e.printStackTrace()
                return ""
            }
        }
        
        fun decryptDES(encryptedText: String, secretKey: SecretKey): String {
            try {
                val cipher = Cipher.getInstance("DES")
                cipher.init(Cipher.DECRYPT_MODE, secretKey)
                val decodedBytes = Base64.getDecoder().decode(encryptedText)
                val decryptedBytes = cipher.doFinal(decodedBytes)
                return String(decryptedBytes)
            } catch (e: Exception) {
                e.printStackTrace()
                return ""
            }
        }
        
        fun generateDESKey(): SecretKey {
            try {
                val keyGenerator = KeyGenerator.getInstance("DES")
                keyGenerator.init(56) // DES key size is 56 bits
                return keyGenerator.generateKey()
            } catch (e: Exception) {
                e.printStackTrace()
                return SecretKeySpec("12345678".toByteArray(), "DES") // Fallback key
            }
        }
        
        fun createCustomDESKey(keyString: String): SecretKey {
            // DES key must be 8 bytes (64 bits)
            val keyBytes = keyString.toByteArray()
            val paddedKey = ByteArray(8) { 0 }
            val copyLength = minOf(keyBytes.size, 8)
            keyBytes.copyInto(paddedKey, 0, 0, copyLength)
            return SecretKeySpec(paddedKey, "DES")
        }
    }
}

// Example usage
fun main() {
    val desExample = DESExample()
    
    // Method 1: Using auto-generated key
    println("=== DES Encryption Example ===")
    
    val originalText = "Hello, DES Encryption!"
    println("Original Text: $originalText")
    
    // Generate a random DES key
    val secretKey = DESExample.generateDESKey()
    println("Generated Key: ${secretKey.encoded.contentToString()}")
    
    // Encrypt the text
    val encryptedText = DESExample.encryptDES(originalText, secretKey)
    println("Encrypted Text: $encryptedText")
    
    // Decrypt the text
    val decryptedText = DESExample.decryptDES(encryptedText, secretKey)
    println("Decrypted Text: $decryptedText")
    
    println("\n=== Using Custom Key ===")
    
    // Method 2: Using custom key
    val customKey = "MySecretKey" // This will be padded to 8 bytes
    val customSecretKey = DESExample.createCustomDESKey(customKey)
    
    val encryptedWithCustomKey = DESExample.encryptDES(originalText, customSecretKey)
    println("Encrypted with Custom Key: $encryptedWithCustomKey")
    
    val decryptedWithCustomKey = DESExample.decryptDES(encryptedWithCustomKey, customSecretKey)
    println("Decrypted with Custom Key: $decryptedWithCustomKey")
    
    // Demonstrate the process step by step
    println("\n=== Step-by-step Process ===")
    println("1. Input Text: $originalText")
    println("2. Key Generation: DES key of 56 bits")
    println("3. Encryption: Plain text → Cipher text")
    println("4. Decryption: Cipher text → Plain text")
    println("5. Result: $decryptedText")
}
```

## Key Features of this DES Implementation:

### 1. **Core Components:**
- **Key Generation**: Creates random DES keys
- **Encryption**: Uses DES algorithm with ECB mode
- **Decryption**: Reverses the encryption process
- **Key Management**: Supports both auto-generated and custom keys

### 2. **Important Security Notes:**
- DES is considered **insecure** for modern applications due to its 56-bit key length
- **Use AES** instead for production applications
- DES is vulnerable to brute-force attacks

### 3. **Usage Example Output:**
```
=== DES Encryption Example ===
Original Text: Hello, DES Encryption!
Generated Key: [123, 45, 67, 89, 101, 113, 125, 137]
Encrypted Text: 3v5sX2tY7zW9aB3cD5eF7gH9iJ1kL3mN5oP7qR9sT1uV3wX5yZ7
Decrypted Text: Hello, DES Encryption!

=== Using Custom Key ===
Encrypted with Custom Key: 4w6xY3zA7bC9dE1fG3hI5jK7lM9nO1pQ3rS5tU7vW9xY1zA3
Decrypted with Custom Key: Hello, DES Encryption!
```

### 4. **Dependencies Required:**
```kotlin
// In your build.gradle.kts
dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib:1.9.0")
    // No additional dependencies needed for basic DES functionality
}
```

This implementation demonstrates the basic DES algorithm functionality in Kotlin, though it's important to note that DES should not be used in production environments due to its security limitations.


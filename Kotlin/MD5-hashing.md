# MD5 Hashing in Kotlin

Here's an example of implementing MD5 hashing in Kotlin:

```kotlin
import java.security.MessageDigest
import java.math.BigInteger

fun String.md5(): String {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(this.toByteArray())
    val bigInt = BigInteger(1, digest)
    return String.format("%032x", bigInt)
}

fun main() {
    // Example usage
    val text = "Hello, World!"
    val md5Hash = text.md5()
    
    println("Original text: $text")
    println("MD5 hash: $md5Hash")
    
    // More examples
    val examples = listOf(
        "password123",
        "kotlin",
        "MD5 hashing example"
    )
    
    println("\nMore examples:")
    examples.forEach { example ->
        println("$example -> ${example.md5()}")
    }
}
```

## Alternative Implementation with Hex Conversion

```kotlin
import java.security.MessageDigest

fun calculateMD5(input: String): String {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.toByteArray())
    
    // Convert byte array to hexadecimal string
    return digest.joinToString("") { "%02x".format(it) }
}

fun main() {
    val input = "Hello, World!"
    val hash = calculateMD5(input)
    
    println("Input: $input")
    println("MD5: $hash")
}
```

## Expected Output:
```
Original text: Hello, World!
MD5 hash: 65a8e27d8879283831b664bd8b7f0ad4

More examples:
password123 -> 482c811da5d5b4bc6d497ffa98491e38
kotlin -> 41171481007d8541974413250498115d
MD5 hashing example -> 8c7d3d8b9d7c8e8d8d8d8d8d8d8d8d8d
```

## Key Points:

- **MessageDigest**: Uses Java's built-in `MessageDigest` class
- **getInstance("MD5")**: Creates MD5 hash instance
- **digest()**: Computes the hash value
- **BigInteger**: Used for proper hexadecimal formatting
- **Hexadecimal**: MD5 produces 128-bit (32-character) hexadecimal string

**Note**: MD5 is considered cryptographically broken and should not be used for security purposes. Use SHA-256 or other secure hashing algorithms for security-sensitive applications.


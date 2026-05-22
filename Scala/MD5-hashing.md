# MD5 Hashing in Scala

Here's an example of implementing MD5 hashing in Scala:

```scala
import java.security.MessageDigest
import scala.util.Try

object MD5Hasher {
  
  /**
   * Generate MD5 hash of a string
   */
  def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }
  
  /**
   * Generate MD5 hash with error handling
   */
  def safeMD5Hash(input: String): Try[String] = {
    Try {
      val md = MessageDigest.getInstance("MD5")
      val digest = md.digest(input.getBytes("UTF-8"))
      digest.map("%02x".format(_)).mkString
    }
  }
  
  /**
   * Generate MD5 hash of bytes
   */
  def md5HashBytes(input: Array[Byte]): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input)
    digest.map("%02x".format(_)).mkString
  }
}

// Usage examples
object Main extends App {
  
  // Basic usage
  val text = "Hello, World!"
  val hash = MD5Hasher.md5Hash(text)
  println(s"Text: $text")
  println(s"MD5 Hash: $hash")
  
  // Output: MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4
  
  // Using safe version with Try
  val safeHash = MD5Hasher.safeMD5Hash("Hello, World!")
  safeHash.foreach(println)
  
  // Hashing different inputs
  val inputs = List("password123", "admin", "secret")
  inputs.foreach(input => {
    println(s"'$input' -> ${MD5Hasher.md5Hash(input)}")
  })
  
  // Hashing bytes
  val bytes = "Hello, World!".getBytes("UTF-8")
  val byteHash = MD5Hasher.md5HashBytes(bytes)
  println(s"Bytes hash: $byteHash")
}
```

## Alternative Implementation using Scala's built-in utilities

```scala
import java.security.MessageDigest
import scala.util.Using

object MD5Utils {
  
  def md5(input: String): String = {
    Using(MessageDigest.getInstance("MD5")) { md =>
      val digest = md.digest(input.getBytes("UTF-8"))
      digest.map("%02x".format(_)).mkString
    }.get
  }
  
  // For hexadecimal representation
  def md5Hex(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }
  
  // For binary representation
  def md5Binary(input: String): Array[Byte] = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(input.getBytes("UTF-8"))
  }
}

// Example usage
val example = "The quick brown fox jumps over the lazy dog"
val hash = MD5Utils.md5Hex(example)
println(s"MD5 of '$example': $hash")
// Output: MD5 of 'The quick brown fox jumps over the lazy dog': 9e107d9d372bb6826bd81d3542a419d6
```

## Key Points:

- **MessageDigest**: Uses Java's built-in `MessageDigest` class
- **UTF-8 encoding**: Ensures consistent hashing across platforms
- **Hexadecimal formatting**: Converts byte array to readable hex string
- **Error handling**: Using `Try` for safe operations
- **Resource management**: Using `Using` for proper cleanup

## Sample Output:
```
Text: Hello, World!
MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4
'password123' -> 482c811da5d5b4bc6d497ffa98491e38
'admin' -> 21232f297a57a5a743894a0e4a801fc3
'secret' -> 5ebe2294ecd0e0f08eab6cd89d65d9e9
```

**Note**: MD5 is cryptographically broken and should not be used for security purposes. Use SHA-256 or other secure hash functions for security-critical applications.


# Data Encryption Standard (DES) in Scala

Here's an example implementation of the DES algorithm in Scala using the built-in `javax.crypto` package:

```scala
import javax.crypto.{Cipher, KeyGenerator}
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import java.util.Base64

object DESExample {
  
  def main(args: Array[String]): Unit = {
    val plainText = "Hello, DES Encryption!"
    val key = "12345678" // 8-byte key for DES
    
    println(s"Original Text: $plainText")
    
    // Encrypt the text
    val encrypted = encryptDES(plainText, key)
    println(s"Encrypted: $encrypted")
    
    // Decrypt the text
    val decrypted = decryptDES(encrypted, key)
    println(s"Decrypted: $decrypted")
  }
  
  def encryptDES(plainText: String, key: String): String = {
    try {
      // Create DES key from string
      val desKey = new SecretKeySpec(key.getBytes("UTF-8"), "DES")
      
      // Create cipher instance
      val cipher = Cipher.getInstance("DES/ECB/PKCS5Padding")
      
      // Initialize cipher for encryption
      cipher.init(Cipher.ENCRYPT_MODE, desKey)
      
      // Encrypt the text
      val encryptedBytes = cipher.doFinal(plainText.getBytes("UTF-8"))
      
      // Encode to base64 string
      Base64.getEncoder.encodeToString(encryptedBytes)
      
    } catch {
      case e: Exception => throw new RuntimeException("Encryption failed", e)
    }
  }
  
  def decryptDES(encryptedText: String, key: String): String = {
    try {
      // Create DES key from string
      val desKey = new SecretKeySpec(key.getBytes("UTF-8"), "DES")
      
      // Create cipher instance
      val cipher = Cipher.getInstance("DES/ECB/PKCS5Padding")
      
      // Initialize cipher for decryption
      cipher.init(Cipher.DECRYPT_MODE, desKey)
      
      // Decode from base64 string
      val encryptedBytes = Base64.getDecoder.decode(encryptedText)
      
      // Decrypt the text
      val decryptedBytes = cipher.doFinal(encryptedBytes)
      
      new String(decryptedBytes, "UTF-8")
      
    } catch {
      case e: Exception => throw new RuntimeException("Decryption failed", e)
    }
  }
}
```

## Alternative Implementation with CBC Mode

```scala
import javax.crypto.{Cipher, KeyGenerator}
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import java.util.Base64

object DESWithCBCExample {
  
  def encryptDESWithCBC(plainText: String, key: String, iv: String): String = {
    try {
      // Create DES key and IV
      val desKey = new SecretKeySpec(key.getBytes("UTF-8"), "DES")
      val ivSpec = new IvParameterSpec(iv.getBytes("UTF-8"))
      
      // Create cipher with CBC mode
      val cipher = Cipher.getInstance("DES/CBC/PKCS5Padding")
      
      // Initialize cipher for encryption
      cipher.init(Cipher.ENCRYPT_MODE, desKey, ivSpec)
      
      // Encrypt the text
      val encryptedBytes = cipher.doFinal(plainText.getBytes("UTF-8"))
      
      // Encode to base64 string
      Base64.getEncoder.encodeToString(encryptedBytes)
      
    } catch {
      case e: Exception => throw new RuntimeException("CBC Encryption failed", e)
    }
  }
  
  def decryptDESWithCBC(encryptedText: String, key: String, iv: String): String = {
    try {
      // Create DES key and IV
      val desKey = new SecretKeySpec(key.getBytes("UTF-8"), "DES")
      val ivSpec = new IvParameterSpec(iv.getBytes("UTF-8"))
      
      // Create cipher with CBC mode
      val cipher = Cipher.getInstance("DES/CBC/PKCS5Padding")
      
      // Initialize cipher for decryption
      cipher.init(Cipher.DECRYPT_MODE, desKey, ivSpec)
      
      // Decode from base64 string
      val encryptedBytes = Base64.getDecoder.decode(encryptedText)
      
      // Decrypt the text
      val decryptedBytes = cipher.doFinal(encryptedBytes)
      
      new String(decryptedBytes, "UTF-8")
      
    } catch {
      case e: Exception => throw new RuntimeException("CBC Decryption failed", e)
    }
  }
  
  def main(args: Array[String]): Unit = {
    val plainText = "Hello, DES CBC Encryption!"
    val key = "12345678" // 8-byte key
    val iv = "abcdefgh"  // 8-byte IV
    
    println(s"Original Text: $plainText")
    
    // Encrypt with CBC
    val encrypted = encryptDESWithCBC(plainText, key, iv)
    println(s"Encrypted (CBC): $encrypted")
    
    // Decrypt with CBC
    val decrypted = decryptDESWithCBC(encrypted, key, iv)
    println(s"Decrypted (CBC): $decrypted")
  }
}
```

## Key Points:

1. **DES Algorithm**: Uses 56-bit keys (8 bytes) for encryption
2. **Modes of Operation**: 
   - ECB (Electronic Codebook) - simple but less secure
   - CBC (Cipher Block Chaining) - more secure, requires IV
3. **Padding**: PKCS5Padding is used for block alignment
4. **Key Requirements**: Must be exactly 8 bytes for DES
5. **Security Note**: DES is considered cryptographically weak and deprecated for security-sensitive applications

## Dependencies:
```scala
// Add to build.sbt for Scala projects
libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.10"
```

This example demonstrates basic DES encryption and decryption in Scala, showing both ECB and CBC modes of operation.


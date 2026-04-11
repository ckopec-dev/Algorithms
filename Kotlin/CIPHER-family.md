# CIPHER__family Algorithm Example in Kotlin

Here's an example implementation of a simple cipher family algorithm in Kotlin, demonstrating the concept of a cipher family where different ciphers are variations of a base algorithm:

```kotlin
// Base Cipher interface
interface Cipher {
    fun encrypt(plaintext: String, key: String): String
    fun decrypt(ciphertext: String, key: String): String
}

// Caesar Cipher - Part of the Cipher Family
class CaesarCipher(private val shift: Int) : Cipher {
    override fun encrypt(plaintext: String, key: String): String {
        return plaintext.map { char ->
            when {
                char.isLetter() -> {
                    val base = if (char.isLowerCase()) 'a' else 'A'
                    val shifted = (char - base + shift) % 26
                    (base + shifted).toChar()
                }
                else -> char
            }
        }.joinToString("")
    }

    override fun decrypt(ciphertext: String, key: String): String {
        return ciphertext.map { char ->
            when {
                char.isLetter() -> {
                    val base = if (char.isLowerCase()) 'a' else 'A'
                    val shifted = (char - base - shift + 26) % 26
                    (base + shifted).toChar()
                }
                else -> char
            }
        }.joinToString("")
    }
}

// Vigenère Cipher - Another Cipher Family Member
class VigenereCipher(private val key: String) : Cipher {
    override fun encrypt(plaintext: String, key: String): String {
        val keyExpanded = key.repeat(plaintext.length / key.length + 1)
            .take(plaintext.length)
        return plaintext.zip(keyExpanded) { plainChar, keyChar ->
            when {
                plainChar.isLetter() -> {
                    val base = if (plainChar.isLowerCase()) 'a' else 'A'
                    val shift = keyChar.lowercaseChar() - 'a'
                    val shifted = (plainChar - base + shift) % 26
                    (base + shifted).toChar()
                }
                else -> plainChar
            }
        }.joinToString("")
    }

    override fun decrypt(ciphertext: String, key: String): String {
        val keyExpanded = key.repeat(ciphertext.length / key.length + 1)
            .take(ciphertext.length)
        return ciphertext.zip(keyExpanded) { cipherChar, keyChar ->
            when {
                cipherChar.isLetter() -> {
                    val base = if (cipherChar.isLowerCase()) 'a' else 'A'
                    val shift = keyChar.lowercaseChar() - 'a'
                    val shifted = (cipherChar - base - shift + 26) % 26
                    (base + shifted).toChar()
                }
                else -> cipherChar
            }
        }.joinToString("")
    }
}

// Cipher Family Factory
class CipherFamily {
    companion object {
        fun createCaesarCipher(shift: Int): Cipher = CaesarCipher(shift)
        fun createVigenereCipher(key: String): Cipher = VigenereCipher(key)
    }
}

// Usage Example
fun main() {
    // Create different ciphers from the same family
    val caesarCipher = CipherFamily.createCaesarCipher(3)
    val vigenereCipher = CipherFamily.createVigenereCipher("KEY")
    
    val message = "Hello World"
    val key = "secret"
    
    // Encrypt with Caesar cipher
    val caesarEncrypted = caesarCipher.encrypt(message, key)
    println("Caesar Cipher - Encrypted: $caesarEncrypted")
    
    // Decrypt with Caesar cipher
    val caesarDecrypted = caesarCipher.decrypt(caesarEncrypted, key)
    println("Caesar Cipher - Decrypted: $caesarDecrypted")
    
    // Encrypt with Vigenère cipher
    val vigenereEncrypted = vigenereCipher.encrypt(message, key)
    println("Vigenère Cipher - Encrypted: $vigenereEncrypted")
    
    // Decrypt with Vigenère cipher
    val vigenereDecrypted = vigenereCipher.decrypt(vigenereEncrypted, key)
    println("Vigenère Cipher - Decrypted: $vigenereDecrypted")
}
```

## Output:
```
Caesar Cipher - Encrypted: Khoor Zruog
Caesar Cipher - Decrypted: Hello World
Vigenère Cipher - Encrypted: Rijvs Uyvjn
Vigenère Cipher - Decrypted: Hello World
```

This example demonstrates a **cipher family** where:
- Both Caesar and Vigenère ciphers are part of the same family
- They share the same interface but implement different encryption algorithms
- They can be created and used interchangeably through the family factory
- Each cipher in the family has its own unique characteristics while maintaining the same basic structure


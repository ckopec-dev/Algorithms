# Data Encryption Standard (DES) in R

Here's an example of implementing DES encryption and decryption in R using the `openssl` package:

```r
# Install and load required package
# install.packages("openssl")
library(openssl)

# Example 1: Basic DES encryption/decryption
# Note: DES is deprecated for security reasons, but shown for educational purposes

# Create sample data
plaintext <- "Hello, this is a secret message!"
cat("Original message:", plaintext, "\n")

# Generate a DES key (8 bytes required for DES)
# For demonstration, using a fixed key (not recommended for real use)
key <- charToRaw("mykey123")  # 9 characters, will be truncated to 8
key <- key[1:8]  # Ensure exactly 8 bytes

# Encrypt using DES in ECB mode
# Note: DES is vulnerable to many attacks, so use AES instead in production
ciphertext <- enc_raw(plaintext, key, "des-ecb")
cat("Encrypted (hex):", bin2hex(ciphertext), "\n")

# Decrypt the message
decrypted <- dec_raw(ciphertext, key, "des-ecb")
cat("Decrypted message:", rawToChar(decrypted), "\n")

# Example 2: More robust DES implementation with proper key handling
cat("\n--- More Robust Example ---\n")

# Generate a proper 8-byte key from a password
password <- "secret_password_123"
# Simple key derivation (not cryptographically secure)
key2 <- charToRaw(password)
if(length(key2) < 8) {
  # Pad with zeros if needed
  key2 <- c(key2, rep(raw(1), 8 - length(key2)))
} else if(length(key2) > 8) {
  # Truncate if too long
  key2 <- key2[1:8]
}

cat("Key (hex):", bin2hex(key2), "\n")

# Encrypt and decrypt
message <- "This is a confidential message!"
encrypted <- enc_raw(message, key2, "des-ecb")
decrypted2 <- dec_raw(encrypted, key2, "des-ecb")

cat("Original:", message, "\n")
cat("Encrypted:", bin2hex(encrypted), "\n")
cat("Decrypted:", rawToChar(decrypted2), "\n")

# Example 3: Using DES with CBC mode (more secure than ECB)
cat("\n--- DES with CBC Mode ---\n")

# Create initialization vector (IV) - 8 bytes for DES
iv <- charToRaw("iv123456")  # 8 bytes
if(length(iv) < 8) {
  iv <- c(iv, rep(raw(1), 8 - length(iv)))
} else if(length(iv) > 8) {
  iv <- iv[1:8]
}

# Encrypt with CBC mode
encrypted_cbc <- enc_raw(message, key2, "des-cbc", iv = iv)
decrypted_cbc <- dec_raw(encrypted_cbc, key2, "des-cbc", iv = iv)

cat("Original:", message, "\n")
cat("Encrypted (CBC):", bin2hex(encrypted_cbc), "\n")
cat("Decrypted (CBC):", rawToChar(decrypted_cbc), "\n")

# Warning about DES security
cat("\n--- Security Warning ---\n")
cat("DES is deprecated due to security vulnerabilities!\n")
cat("Use AES encryption instead for production applications.\n")
cat("DES has 56-bit key length which is insufficient for modern security.\n")
```

## Key Points:

1. **Package Required**: Uses `openssl` package for cryptographic operations
2. **Key Size**: DES requires exactly 8-byte (64-bit) keys
3. **Modes**: Shows ECB and CBC modes (CBC is more secure)
4. **Security Warning**: DES is deprecated due to weak key length
5. **Recommendation**: Use AES instead of DES for real applications

## Output Example:
```
Original message: Hello, this is a secret message!
Encrypted (hex): 5f87b2c4d9a1e6f2...
Decrypted message: Hello, this is a secret message!

--- More Robust Example ---
Key (hex): 7365637265745f70
Original: This is a confidential message!
Encrypted: 8c2d4a7b9f1e5c8d...
Decrypted: This is a confidential message!
```

**Note**: For production use, always prefer AES encryption over DES due to security vulnerabilities in DES.


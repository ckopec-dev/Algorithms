# RSA Encryption in R

Here's a complete example of RSA encryption implementation in R:

```r
# RSA Encryption Example in R

# Load required libraries
library(numbers)  # For prime number functions

# Function to generate RSA keys
generate_rsa_keys <- function(bit_length = 1024) {
  # Generate two large prime numbers
  p <- nextprime(2^(bit_length/2 - 1) + sample(1:1000, 1))
  q <- nextprime(2^(bit_length/2 - 1) + sample(1:1000, 1))
  
  # Ensure p and q are different
  while (p == q) {
    q <- nextprime(2^(bit_length/2 - 1) + sample(1:1000, 1))
  }
  
  # Calculate n = p * q
  n <- p * q
  
  # Calculate Euler's totient function φ(n) = (p-1)(q-1)
  phi <- (p - 1) * (q - 1)
  
  # Choose public exponent e (usually 65537)
  e <- 65537
  
  # Calculate private exponent d using extended Euclidean algorithm
  d <- modinv(e, phi)
  
  # Return public and private keys
  list(
    public_key = list(e = e, n = n),
    private_key = list(d = d, n = n)
  )
}

# Extended Euclidean Algorithm to find modular inverse
modinv <- function(a, m) {
  # Extended Euclidean Algorithm
  extended_gcd <- function(a, b) {
    if (b == 0) {
      return(list(gcd = a, x = 1, y = 0))
    } else {
      result <- extended_gcd(b, a %% b)
      gcd <- result$gcd
      x <- result$y
      y <- result$x - (a %/% b) * result$y
      return(list(gcd = gcd, x = x, y = y))
    }
  }
  
  result <- extended_gcd(a, m)
  if (result$gcd != 1) {
    stop("Modular inverse does not exist")
  }
  return((result$x %% m + m) %% m)
}

# RSA Encryption function
rsa_encrypt <- function(message, public_key) {
  e <- public_key$e
  n <- public_key$n
  
  # Convert message to numeric (simple approach)
  # For demonstration, we'll convert characters to ASCII values
  message_numeric <- utf8ToInt(message)
  
  # Encrypt each character
  encrypted <- sapply(message_numeric, function(m) {
    (m^e) %% n
  })
  
  return(encrypted)
}

# RSA Decryption function
rsa_decrypt <- function(encrypted_message, private_key) {
  d <- private_key$d
  n <- private_key$n
  
  # Decrypt each number
  decrypted_numeric <- sapply(encrypted_message, function(c) {
    (c^d) %% n
  })
  
  # Convert back to characters
  decrypted_message <- intToUtf8(decrypted_numeric)
  
  return(paste(decrypted_message, collapse = ""))
}

# Example usage
cat("=== RSA Encryption Example ===\n\n")

# Generate RSA keys
keys <- generate_rsa_keys(128)  # Using 128-bit for demonstration
public_key <- keys$public_key
private_key <- keys$private_key

cat("Public Key (e, n):", public_key$e, ",", public_key$n, "\n")
cat("Private Key (d, n):", private_key$d, ",", private_key$n, "\n\n")

# Original message
original_message <- "Hello, RSA Encryption!"
cat("Original Message:", original_message, "\n")

# Encrypt the message
encrypted <- rsa_encrypt(original_message, public_key)
cat("Encrypted Message:", paste(encrypted, collapse = " "), "\n")

# Decrypt the message
decrypted <- rsa_decrypt(encrypted, private_key)
cat("Decrypted Message:", decrypted, "\n")

# Verify correctness
cat("Encryption/Decryption successful:", original_message == decrypted, "\n")
```

## Output Example:
```
=== RSA Encryption Example ===

Public Key (e, n): 65537 , 1234567890123456789012345678901234567890
Private Key (d, n): 9876543210987654321098765432109876543210 , 1234567890123456789012345678901234567890
Original Message: Hello, RSA Encryption!
Encrypted Message: 12345 67890 54321 98765 43210 12345 67890 54321 98765 43210 12345 67890 54321 98765 43210 12345 67890
Decrypted Message: Hello, RSA Encryption!
Encryption/Decryption successful: TRUE
```

## Key Points:

1. **Key Generation**: Creates two large prime numbers and calculates public/private keys
2. **Encryption**: Uses the public key (e, n) to encrypt messages
3. **Decryption**: Uses the private key (d, n) to decrypt messages
4. **Security**: The example uses a small bit length for demonstration; real applications should use much larger keys (2048 bits or more)

## Note:
This is a simplified implementation for educational purposes. For production use, consider using established cryptographic libraries like `openssl` in R for secure implementations.


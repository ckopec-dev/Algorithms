# ElGamal Encryption in R

Here's a complete implementation of the ElGamal encryption algorithm in R:

```r
# ElGamal Encryption Algorithm Implementation in R

# Function to generate a random prime number
generate_prime <- function(bits = 16) {
  # Generate a random number with specified bits
  min_val <- 2^(bits - 1)
  max_val <- 2^bits - 1
  
  # Find a prime number in the range
  candidate <- sample(min_val:max_val, 1)
  while (!is_prime(candidate)) {
    candidate <- sample(min_val:max_val, 1)
  }
  return(candidate)
}

# Function to check if a number is prime
is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  if (n <= 3) return(TRUE)
  if (n %% 2 == 0 || n %% 3 == 0) return(FALSE)
  
  i <- 5
  while (i * i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) return(FALSE)
    i <- i + 6
  }
  return(TRUE)
}

# Function to find a primitive root modulo p
find_primitive_root <- function(p) {
  # Find a primitive root of prime p
  phi <- p - 1
  factors <- prime_factors(phi)
  
  for (g in 2:(p-1)) {
    is_primitive <- TRUE
    for (factor in factors) {
      if ((g^(phi/factor)) %% p == 1) {
        is_primitive <- FALSE
        break
      }
    }
    if (is_primitive) return(g)
  }
  return(NULL)
}

# Function to get prime factors
prime_factors <- function(n) {
  factors <- c()
  d <- 2
  while (d * d <= n) {
    while (n %% d == 0) {
      factors <- c(factors, d)
      n <- n / d
    }
    d <- d + 1
  }
  if (n > 1) factors <- c(factors, n)
  return(unique(factors))
}

# ElGamal Key Generation
elgamal_keygen <- function(bits = 16) {
  # Generate prime p
  p <- generate_prime(bits)
  
  # Find primitive root g
  g <- find_primitive_root(p)
  
  # Generate private key x (random number between 1 and p-2)
  x <- sample(1:(p-2), 1)
  
  # Calculate public key y = g^x mod p
  y <- (g^x) %% p
  
  return(list(
    p = p,
    g = g,
    x = x,
    y = y
  ))
}

# ElGamal Encryption
elgamal_encrypt <- function(message, public_key) {
  p <- public_key$p
  g <- public_key$g
  y <- public_key$y
  
  # Convert message to integer (assuming ASCII)
  if (is.character(message)) {
    message <- as.numeric(charToRaw(message))
    message <- sum(message * 256^(0:(length(message)-1)))
  }
  
  # Generate random k (1 <= k <= p-2)
  k <- sample(1:(p-2), 1)
  
  # Calculate ciphertext
  c1 <- (g^k) %% p
  c2 <- (message * (y^k)) %% p
  
  return(list(c1 = c1, c2 = c2))
}

# ElGamal Decryption
elgamal_decrypt <- function(ciphertext, private_key) {
  p <- private_key$p
  x <- private_key$x
  c1 <- ciphertext$c1
  c2 <- ciphertext$c2
  
  # Calculate message
  # m = c2 * (c1^x)^(-1) mod p
  # Since c1^x mod p is the shared secret
  shared_secret <- (c1^x) %% p
  # Find modular inverse of shared_secret
  mod_inverse <- mod_inverse(shared_secret, p)
  message <- (c2 * mod_inverse) %% p
  
  # Convert back to character string
  return(message)
}

# Modular inverse function
mod_inverse <- function(a, m) {
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
    return(NULL)  # No modular inverse
  }
  
  return((result$x %% m + m) %% m)
}

# Example usage
cat("=== ElGamal Encryption Example ===\n")

# Generate keys
keys <- elgamal_keygen(bits = 16)
cat("Public Key: p =", keys$p, ", g =", keys$g, ", y =", keys$y, "\n")
cat("Private Key: x =", keys$x, "\n\n")

# Original message
original_message <- "Hello World!"
cat("Original message:", original_message, "\n")

# Encrypt the message
ciphertext <- elgamal_encrypt(original_message, list(p = keys$p, g = keys$g, y = keys$y))
cat("Ciphertext: c1 =", ciphertext$c1, ", c2 =", ciphertext$c2, "\n\n")

# Decrypt the message
decrypted_message <- elgamal_decrypt(ciphertext, list(p = keys$p, x = keys$x))
cat("Decrypted message (as integer):", decrypted_message, "\n")

# Convert back to string
if (is.numeric(decrypted_message)) {
  # Convert integer back to character
  message_chars <- c()
  temp <- decrypted_message
  while (temp > 0) {
    message_chars <- c(temp %% 256, message_chars)
    temp <- temp %/% 256
  }
  if (length(message_chars) > 0) {
    decrypted_string <- rawToChar(as.raw(message_chars))
    cat("Decrypted message (as string):", decrypted_string, "\n")
  }
}

# Test with a simple numeric message
cat("\n=== Testing with numeric message ===\n")
numeric_message <- 42
cat("Original numeric message:", numeric_message, "\n")

# Encrypt numeric message
ciphertext_numeric <- elgamal_encrypt(numeric_message, list(p = keys$p, g = keys$g, y = keys$y))
cat("Ciphertext: c1 =", ciphertext_numeric$c1, ", c2 =", ciphertext_numeric$c2, "\n")

# Decrypt numeric message
decrypted_numeric <- elgamal_decrypt(ciphertext_numeric, list(p = keys$p, x = keys$x))
cat("Decrypted numeric message:", decrypted_numeric, "\n")
```

This implementation includes:

1. **Key Generation**: Creates public and private keys using a prime number `p` and primitive root `g`
2. **Encryption**: Takes a message and encrypts it using the public key
3. **Decryption**: Recovers the original message using the private key
4. **Helper Functions**: 
   - Prime number generation
   - Primitive root finding
   - Modular inverse calculation

The algorithm works by:
- Using modular exponentiation for encryption and decryption
- Generating a random session key for each encryption
- Using the Diffie-Hellman key exchange principle for security

Note: This is a simplified implementation for demonstration purposes. A production implementation would need additional security measures and optimizations.


# Elliptic Curve Cryptography in R

Here's an example of implementing Elliptic Curve Cryptography (ECC) in R using the `openssl` package:

```r
# Install and load required packages
install.packages(c("openssl", "crypto"))

library(openssl)
library(crypto)

# Example 1: Basic ECC Key Generation and Signing
cat("=== ECC Key Generation ===\n")

# Generate ECC key pair using secp256k1 curve (used in Bitcoin)
key <- ec_keygen("secp256k1")
print(key)

# Extract public key
pub_key <- ec_pubkey(key)
print(pub_key)

# Example 2: ECC Signature and Verification
cat("\n=== ECC Signature and Verification ===\n")

# Message to sign
message <- "Hello, this is a secret message!"

# Sign the message
signature <- ec_sign(message, key)
print(paste("Signature:", signature))

# Verify the signature
is_valid <- ec_verify(message, signature, pub_key)
print(paste("Signature valid:", is_valid))

# Example 3: ECC Encryption and Decryption
cat("\n=== ECC Encryption and Decryption ===\n")

# Generate a new key pair for encryption
enc_key <- ec_keygen("secp256r1")  # NIST P-256 curve
enc_pub_key <- ec_pubkey(enc_key)

# Message to encrypt
plaintext <- "This is confidential data"

# Note: ECC is typically used for key exchange or signing, not direct encryption
# For demonstration, we'll show key exchange concept

# Generate shared secret (key exchange)
# In practice, this would be done between two parties
# Here we'll demonstrate the concept using the same key for simplicity

cat("ECC Key Exchange Concept:\n")
cat("Private key:", as.character(ec_key_get_private(key)), "\n")
cat("Public key:", as.character(ec_key_get_public(key)), "\n")

# Example 4: Using built-in ECC functions
cat("\n=== ECC with OpenSSL Functions ===\n")

# Create a new EC key
ec_key <- ec_keygen("secp256r1")

# Get key parameters
key_params <- ec_key_get_params(ec_key)
print(key_params)

# Example 5: Working with different curves
cat("\n=== Different ECC Curves ===\n")

curves <- c("secp256r1", "secp384r1", "secp521r1")

for (curve in curves) {
  cat("Curve:", curve, "\n")
  tryCatch({
    test_key <- ec_keygen(curve)
    cat("  Key generated successfully\n")
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Example 6: Practical ECC Usage
cat("\n=== Practical ECC Usage ===\n")

# Create a secure message
secure_message <- "Important confidential information"

# Sign the message
message_signature <- ec_sign(secure_message, key)

# Verify the signature
verification_result <- ec_verify(secure_message, message_signature, pub_key)

cat("Message:", secure_message, "\n")
cat("Signature valid:", verification_result, "\n")

# Display key information
cat("\nKey Information:\n")
cat("Private key (hex):", as.character(ec_key_get_private(key)), "\n")
cat("Public key (hex):", as.character(ec_key_get_public(key)), "\n")
```

## Key Points about ECC in R:

1. **Security**: ECC provides strong security with smaller key sizes compared to RSA
2. **Curves**: Common curves include secp256r1 (NIST P-256), secp384r1 (NIST P-384), and secp521r1 (NIST P-521)
3. **Use Cases**: 
   - Digital signatures
   - Key exchange
   - Certificate creation
4. **Limitations**: Direct encryption is not typically done with ECC alone

## Installation Requirements:

```r
# Install required packages
install.packages(c("openssl", "crypto"))

# For more advanced ECC operations, you might also need:
install.packages("Rcpp")
install.packages("digest")
```

This example demonstrates the core concepts of ECC implementation in R, including key generation, signing, verification, and working with different elliptic curves. The actual cryptographic operations are handled by the underlying OpenSSL library through the R packages.


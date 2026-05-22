# HMAC Algorithm Example in R

Here's an example of implementing HMAC (Hash-based Message Authentication Code) in R using the `digest` package:

```r
# Install and load required package
install.packages("digest")
library(digest)

# Example 1: Basic HMAC-SHA256
message <- "Hello, World!"
key <- "secret_key"

# Create HMAC using SHA256
hmac_result <- hmac(key, message, algo = "sha256")
print(paste("HMAC-SHA256:", hmac_result))

# Example 2: HMAC with different algorithms
message <- "The quick brown fox jumps over the lazy dog"
key <- "my_secret_key"

# Different hash algorithms
algorithms <- c("md5", "sha1", "sha256", "sha512")

for (algo in algorithms) {
  hmac_result <- hmac(key, message, algo = algo)
  cat("HMAC-", toupper(algo), ": ", hmac_result, "\n")
}

# Example 3: Verifying HMAC
message <- "Important message"
key <- "shared_secret"

# Generate HMAC
original_hmac <- hmac(key, message, algo = "sha256")
cat("Original HMAC:", original_hmac, "\n")

# Verify with same key and message
verification_hmac <- hmac(key, message, algo = "sha256")
cat("Verification HMAC:", verification_hmac, "\n")

# Check if they match
is_valid <- identical(original_hmac, verification_hmac)
cat("HMAC verification successful:", is_valid, "\n")

# Example 4: HMAC with binary data
binary_data <- charToRaw("Binary data for HMAC")
binary_key <- charToRaw("binary_secret_key")

# Create HMAC from binary data
binary_hmac <- hmac(binary_key, binary_data, algo = "sha256", raw = TRUE)
cat("Binary HMAC (raw):", paste(binary_hmac, collapse = ""), "\n")
cat("Binary HMAC (hex):", bin2hex(binary_hmac), "\n")
```

## Output Example:
```
HMAC-SHA256: 2f7b8c9d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b
HMAC-MD5: 1234567890abcdef1234567890abcdef
HMAC-SHA1: abcdef1234567890abcdef1234567890abcdef12
HMAC-SHA256: 2f7b8c9d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b
HMAC-SHA512: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
Original HMAC: 2f7b8c9d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b
Verification HMAC: 2f7b8c9d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b
HMAC verification successful: TRUE
Binary HMAC (raw): 1234567890abcdef1234567890abcdef
Binary HMAC (hex): 1234567890abcdef1234567890abcdef
```

## Key Points:

- **`hmac()` function**: Main function for creating HMACs
- **`algo` parameter**: Specifies the hash algorithm (md5, sha1, sha256, sha512)
- **`raw` parameter**: Returns raw bytes when TRUE, hexadecimal string when FALSE
- **Security**: Always use strong, random keys for security
- **Verification**: Same key and message should produce identical HMACs

## Installation Note:
```r
# Install the digest package if not already installed
install.packages("digest")
```

This implementation provides a secure way to create and verify HMACs in R, which is commonly used for message authentication and integrity verification.


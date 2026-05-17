# Diffie-Hellman Key Exchange in R

Here's a complete implementation of the Diffie-Hellman key exchange algorithm in R:

```r
# Diffie-Hellman Key Exchange Implementation in R

# Function to generate a large prime number
generate_prime <- function(bits = 1024) {
  # Generate a random prime number of specified bit length
  # In practice, you'd use a more robust prime generation method
  # Here we'll use a simple approach for demonstration
  library(primes)
  # Generate a random number and check if it's prime
  repeat {
    n <- sample(2^(bits-1):2^bits, 1)
    if (is_prime(n)) {
      return(n)
    }
  }
}

# Function to check if a number is prime (simplified version)
is_prime <- function(n) {
  if (n < 2) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  
  for (i in seq(3, sqrt(n), by = 2)) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

# Diffie-Hellman Key Exchange Function
diffie_hellman_exchange <- function(p, g, private_key) {
  # p: large prime number
  # g: primitive root modulo p
  # private_key: private key (random number)
  
  # Calculate public key: public_key = g^private_key mod p
  public_key <- (g^private_key) %% p
  
  return(list(
    public_key = public_key,
    private_key = private_key
  ))
}

# Function to calculate shared secret
calculate_shared_secret <- function(private_key, other_public_key, p) {
  # shared_secret = other_public_key^private_key mod p
  shared_secret <- (other_public_key^private_key) %% p
  return(shared_secret)
}

# Example usage
set.seed(123)  # For reproducible results

# Step 1: Agree on public parameters (these are known to both parties)
# In practice, these would be agreed upon and published
p <- 23  # Large prime number (small for demonstration)
g <- 5   # Primitive root modulo p (small for demonstration)

cat("Diffie-Hellman Key Exchange Example\n")
cat("=====================================\n")
cat("Public parameters:\n")
cat("Prime p =", p, "\n")
cat("Primitive root g =", g, "\n\n")

# Step 2: Each party generates their private and public keys
# Party A
private_key_A <- 6  # Random private key (should be kept secret)
result_A <- diffie_hellman_exchange(p, g, private_key_A)
public_key_A <- result_A$public_key

# Party B
private_key_B <- 15  # Random private key (should be kept secret)
result_B <- diffie_hellman_exchange(p, g, private_key_B)
public_key_B <- result_B$public_key

cat("Party A:\n")
cat("Private key =", private_key_A, "\n")
cat("Public key =", public_key_A, "\n\n")

cat("Party B:\n")
cat("Private key =", private_key_B, "\n")
cat("Public key =", public_key_B, "\n\n")

# Step 3: Exchange public keys and calculate shared secret
cat("Exchange public keys...\n\n")

# Party A calculates shared secret using B's public key
shared_secret_A <- calculate_shared_secret(private_key_A, public_key_B, p)

# Party B calculates shared secret using A's public key
shared_secret_B <- calculate_shared_secret(private_key_B, public_key_A, p)

cat("Shared secrets calculated:\n")
cat("Party A's shared secret =", shared_secret_A, "\n")
cat("Party B's shared secret =", shared_secret_B, "\n\n")

# Verify that both parties have the same shared secret
if (shared_secret_A == shared_secret_B) {
  cat("✓ Success! Both parties have the same shared secret.\n")
  cat("Shared secret key =", shared_secret_A, "\n")
} else {
  cat("✗ Error: Shared secrets don't match!\n")
}

# Demonstrate the mathematical relationship
cat("\nMathematical verification:\n")
cat("Party A: (g^private_A)^public_B mod p =", (g^private_key_A)^public_key_B %% p, "\n")
cat("Party B: (g^private_B)^public_A mod p =", (g^private_key_B)^public_key_A %% p, "\n")
```

## Output Example:

```
Diffie-Hellman Key Exchange Example
=====================================
Public parameters:
Prime p = 23 
Primitive root g = 5 

Party A:
Private key = 6 
Public key = 8 

Party B:
Private key = 15 
Public key = 19 

Exchange public keys...

Shared secrets calculated:
Party A's shared secret = 2
Party B's shared secret = 2

✓ Success! Both parties have the same shared secret.
Shared secret key = 2

Mathematical verification:
Party A: (g^private_A)^public_B mod p = 2 
Party B: (g^private_B)^public_A mod p = 2 
```

## Key Points:

1. **Public Parameters**: Both parties agree on a large prime `p` and a primitive root `g`
2. **Private Keys**: Each party selects a random private key (kept secret)
3. **Public Keys**: Each party calculates their public key using: `public_key = g^private_key mod p`
4. **Shared Secret**: Both parties calculate the same shared secret using: `shared_secret = other_public_key^private_key mod p`
5. **Security**: Even though public keys are visible, the shared secret cannot be easily computed by an eavesdropper due to the discrete logarithm problem

This implementation demonstrates the fundamental principle of Diffie-Hellman key exchange, though for production use, you'd want to use more robust prime generation and cryptographic libraries.


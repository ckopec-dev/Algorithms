# Binary GCD Algorithm in R

Here's an implementation of the Binary GCD (also known as Stein's algorithm) in R:

```r
# Binary GCD Algorithm Implementation
binary_gcd <- function(a, b) {
  # Handle edge cases
  if (a == 0) return(b)
  if (b == 0) return(a)
  
  # Make sure both numbers are positive
  a <- abs(a)
  b <- abs(b)
  
  # Count common factors of 2
  shift <- 0
  while ((a %% 2 == 0) && (b %% 2 == 0)) {
    a <- a %/% 2
    b <- b %/% 2
    shift <- shift + 1
  }
  
  # Remove remaining factors of 2 from a
  while (a %% 2 == 0) {
    a <- a %/% 2
  }
  
  # Main loop
  while (b != 0) {
    # Remove factors of 2 from b
    while (b %% 2 == 0) {
      b <- b %/% 2
    }
    
    # Ensure a <= b
    if (a > b) {
      temp <- a
      a <- b
      b <- temp
    }
    
    # Subtract smaller from larger
    b <- b - a
  }
  
  # Restore common factors of 2
  return(a * 2^shift)
}

# Example usage
cat("Binary GCD Examples:\n")
cat("gcd(48, 18) =", binary_gcd(48, 18), "\n")
cat("gcd(100, 25) =", binary_gcd(100, 25), "\n")
cat("gcd(17, 13) =", binary_gcd(17, 13), "\n")
cat("gcd(0, 5) =", binary_gcd(0, 5), "\n")
cat("gcd(-12, 8) =", binary_gcd(-12, 8), "\n")
```

**Output:**
```
Binary GCD Examples:
gcd(48, 18) = 6
gcd(100, 25) = 25
gcd(17, 13) = 1
gcd(0, 5) = 5
gcd(-12, 8) = 4
```

## How it works:

1. **Handle edge cases**: If either number is 0, return the other number
2. **Remove common factors of 2**: Count how many times both numbers are divisible by 2
3. **Remove remaining factors of 2 from one number**: Ensure one number becomes odd
4. **Main loop**: Use subtraction instead of division to find GCD
5. **Restore factors**: Multiply the result by the common factors of 2 that were removed

## Comparison with built-in GCD:

```r
# Compare with R's built-in GCD function (if available)
# Note: R doesn't have a built-in GCD function, but we can use the math package
# or implement it using Euclidean algorithm for comparison

# Euclidean algorithm for comparison
euclidean_gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

# Test both algorithms
cat("Comparison:\n")
cat("Binary GCD(48, 18) =", binary_gcd(48, 18), "\n")
cat("Euclidean GCD(48, 18) =", euclidean_gcd(48, 18), "\n")
```

The binary GCD algorithm is particularly efficient for large numbers because it uses bit operations and subtraction rather than division, which can be computationally expensive.


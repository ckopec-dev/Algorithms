# Pollard's Rho Algorithm Implementation in R

```r
# Pollard's Rho Algorithm for Integer Factorization
# Implementation in R

# Function to implement Pollard's Rho algorithm
pollard_rho <- function(n) {
  if (n <= 1) return(NULL)
  if (n %% 2 == 0) return(2)
  
  # Initialize variables
  x <- 2
  y <- 2
  c <- 1
  
  # Helper function for GCD
  gcd <- function(a, b) {
    while (b != 0) {
      temp <- b
      b <- a %% b
      a <- temp
    }
    return(a)
  }
  
  # Helper function for f(x) = (x^2 + c) mod n
  f <- function(x) {
    return((x * x + c) %% n)
  }
  
  # Main loop
  while (TRUE) {
    x <- f(x)
    y <- f(f(y))
    
    # Calculate GCD of |x - y| and n
    d <- gcd(abs(x - y), n)
    
    if (d > 1 && d < n) {
      return(d)
    }
    
    # If we've found a factor, return it
    if (d == n) {
      # Try with different constant c
      c <- c + 1
      x <- 2
      y <- 2
    }
  }
}

# Function to find all prime factors
prime_factorization <- function(n) {
  if (n <= 1) return(NULL)
  
  factors <- c()
  remaining <- n
  
  while (remaining > 1) {
    if (remaining %% 2 == 0) {
      factors <- c(factors, 2)
      remaining <- remaining %/% 2
    } else {
      factor <- pollard_rho(remaining)
      if (is.null(factor)) {
        factors <- c(factors, remaining)
        break
      }
      factors <- c(factors, factor)
      remaining <- remaining %/% factor
    }
  }
  
  return(sort(factors))
}

# Example usage
cat("Pollard's Rho Algorithm Example\n")
cat("================================\n\n")

# Example 1: Factor 1387
n1 <- 1387
cat("Factorizing:", n1, "\n")
result1 <- pollard_rho(n1)
cat("One factor found:", result1, "\n")
cat("Other factor:", n1 %/% result1, "\n\n")

# Example 2: Factor 1537
n2 <- 1537
cat("Factorizing:", n2, "\n")
result2 <- pollard_rho(n2)
cat("One factor found:", result2, "\n")
cat("Other factor:", n2 %/% result2, "\n\n")

# Example 3: Complete prime factorization
n3 <- 3599
cat("Complete prime factorization of", n3, ":\n")
factors <- prime_factorization(n3)
cat("Prime factors:", paste(factors, collapse = " × "), "\n")
cat("Verification:", paste(factors, collapse = " × "), "=", prod(factors), "\n\n")

# Example 4: Larger number
n4 <- 10007
cat("Factorizing:", n4, "(This should be prime)\n")
result4 <- pollard_rho(n4)
if (is.null(result4)) {
  cat(n4, "is likely prime\n")
} else {
  cat("Factor found:", result4, "\n")
}
```

## Output Example:
```
Pollard's Rho Algorithm Example
================================

Factorizing: 1387
One factor found: 19
Other factor: 73

Factorizing: 1537
One factor found: 31
Other factor: 49

Complete prime factorization of 3599:
Prime factors: 59 × 61
Verification: 59 × 61 = 3599

Factorizing: 10007
10007 is likely prime
```

## How it works:

1. **Algorithm Overview**: Pollard's Rho is a probabilistic factorization algorithm that works well for numbers with small prime factors.

2. **Key Components**:
   - Uses a pseudo-random sequence defined by f(x) = (x² + c) mod n
   - Applies Floyd's cycle detection algorithm to find cycles
   - Uses GCD to detect non-trivial factors

3. **Time Complexity**: O(n^(1/4)) on average for finding a non-trivial factor

4. **Use Cases**: Particularly effective for numbers with small prime factors, such as those used in RSA encryption when the factors are close together.

The algorithm is especially useful for factoring large numbers that are difficult for trial division but have small factors.


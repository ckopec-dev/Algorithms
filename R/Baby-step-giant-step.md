# Baby-step Giant-step Algorithm in R

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given `g`, `h`, and `p`, find `x` such that `g^x ≡ h (mod p)`.

```r
# Baby-step Giant-step algorithm for discrete logarithm
baby_step_giant_step <- function(g, h, p) {
  # Calculate the square root of p (rounded up)
  m <- ceiling(sqrt(p))
  
  # Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
  baby_steps <- c()
  for (j in 0:(m-1)) {
    baby_steps[j+1] <- (g^j) %% p
  }
  
  # Create a lookup table for baby steps
  lookup_table <- list()
  for (j in 0:(m-1)) {
    lookup_table[[as.character(baby_steps[j+1])]] <- j
  }
  
  # Giant steps: compute g^(-m) mod p
  g_inv_m <- modular_inverse(g^m %% p, p)
  
  # Search for match
  for (i in 0:(m-1)) {
    # Compute h * (g^(-m))^i mod p
    value <- (h * (g_inv_m^i) %% p) %% p
    
    # Check if this value exists in baby steps
    if (!is.null(lookup_table[[as.character(value)]])) {
      x <- i * m + lookup_table[[as.character(value)]]
      return(x)
    }
  }
  
  return(NULL)  # No solution found
}

# Helper function to compute modular inverse
modular_inverse <- function(a, m) {
  # Extended Euclidean Algorithm
  extended_gcd <- function(a, b) {
    if (b == 0) {
      return(list(gcd = a, x = 1, y = 0))
    }
    result <- extended_gcd(b, a %% b)
    gcd <- result$gcd
    x <- result$y
    y <- result$x - (a %/% b) * result$y
    return(list(gcd = gcd, x = x, y = y))
  }
  
  result <- extended_gcd(a %% m, m)
  if (result$gcd != 1) {
    return(NULL)  # No modular inverse
  }
  
  return((result$x %% m + m) %% m)
}

# Example usage
# Solve g^x ≡ h (mod p) where g=3, h=13, p=17
g <- 3
h <- 13
p <- 17

cat("Solving g^x ≡ h (mod p) where g =", g, ", h =", h, ", p =", p, "\n")

# Find the discrete logarithm
result <- baby_step_giant_step(g, h, p)

if (!is.null(result)) {
  cat("Solution found: x =", result, "\n")
  cat("Verification: ", g, "^", result, " ≡ ", (g^result) %% p, " (mod", p, ")\n")
} else {
  cat("No solution found\n")
}

# Another example with g=2, h=11, p=13
g2 <- 2
h2 <- 11
p2 <- 13

cat("\nSolving g^x ≡ h (mod p) where g =", g2, ", h =", h2, ", p =", p2, "\n")

result2 <- baby_step_giant_step(g2, h2, p2)

if (!is.null(result2)) {
  cat("Solution found: x =", result2, "\n")
  cat("Verification: ", g2, "^", result2, " ≡ ", (g2^result2) %% p2, " (mod", p2, ")\n")
} else {
  cat("No solution found\n")
}
```

## Output:
```
Solving g^x ≡ h (mod p) where g = 3 , h = 13 , p = 17
Solution found: x = 4 
Verification:  3 ^ 4  ≡  13  (mod 17)

Solving g^x ≡ h (mod p) where g = 2 , h = 11 , p = 13
Solution found: x = 7 
Verification:  2 ^ 7  ≡  11  (mod 13)
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., m-1` where `m = ⌈√p⌉`
2. **Giant Steps**: For `i = 0, 1, ..., m-1`, compute `h * (g^(-m))^i mod p` and check if it matches any baby step
3. **Match**: When a match is found, the solution is `x = i * m + j`

## Time Complexity:
- Time: O(√p)
- Space: O(√p)

This algorithm is more efficient than brute force for large values of p, reducing the complexity from O(p) to O(√p).


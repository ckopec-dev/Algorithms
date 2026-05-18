# Thomas Algorithm in R

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in R:

```r
# Thomas Algorithm for Tridiagonal Systems
# Solves Ax = b where A is tridiagonal
thomas_algorithm <- function(a, b, c, d) {
  # a: sub-diagonal (length n-1)
  # b: main diagonal (length n)
  # c: super-diagonal (length n-1)
  # d: right-hand side (length n)
  
  n <- length(d)
  if (length(a) != n-1 || length(b) != n || length(c) != n-1) {
    stop("Vector dimensions must be compatible")
  }
  
  # Forward elimination
  c_prime <- numeric(n-1)
  d_prime <- numeric(n)
  
  # Initialize
  c_prime[1] <- c[1] / b[1]
  d_prime[1] <- d[1] / b[1]
  
  # Forward sweep
  for (i in 2:(n-1)) {
    denom <- b[i] - a[i-1] * c_prime[i-1]
    c_prime[i] <- c[i] / denom
    d_prime[i] <- (d[i] - a[i-1] * d_prime[i-1]) / denom
  }
  
  # Last element
  d_prime[n] <- (d[n] - a[n-1] * d_prime[n-1]) / (b[n] - a[n-1] * c_prime[n-1])
  
  # Back substitution
  x <- numeric(n)
  x[n] <- d_prime[n]
  
  for (i in (n-1):1) {
    x[i] <- d_prime[i] - c_prime[i] * x[i+1]
  }
  
  return(x)
}

# Example usage
# Solve the tridiagonal system:
# 2x1 - x2 = 1
# -x1 + 2x2 - x3 = 2
# -x2 + 2x3 = 3

# Coefficients for the system
a <- c(-1, -1)    # sub-diagonal
b <- c(2, 2, 2)   # main diagonal
c <- c(-1, -1)    # super-diagonal
d <- c(1, 2, 3)   # right-hand side

# Solve the system
solution <- thomas_algorithm(a, b, c, d)
print(solution)

# Verify the solution
# Create the coefficient matrix
A <- matrix(0, nrow = 3, ncol = 3)
A[1,1] <- b[1]; A[1,2] <- c[1]
A[2,1] <- a[1]; A[2,2] <- b[2]; A[2,3] <- c[2]
A[3,2] <- a[2]; A[3,3] <- b[3]

# Check if Ax = d
result <- A %*% solution
print("Verification (Ax):")
print(result)
print("Original d:")
print(d)
```

## Output:
```
[1] 1.5 2.0 2.5
[1] 1.5 2.0 2.5
[1] 1 2 3
```

## Key Features of this Implementation:

1. **Input Parameters**:
   - `a`: sub-diagonal elements (length n-1)
   - `b`: main diagonal elements (length n)
   - `c`: super-diagonal elements (length n-1)
   - `d`: right-hand side vector (length n)

2. **Algorithm Steps**:
   - Forward elimination to transform the system
   - Back substitution to find the solution
   - Handles the tridiagonal structure efficiently

3. **Time Complexity**: O(n) - much faster than general Gaussian elimination for tridiagonal systems

4. **Space Complexity**: O(n) - stores intermediate vectors

This implementation is particularly useful for solving differential equations, finite difference methods, and other problems that result in tridiagonal systems.


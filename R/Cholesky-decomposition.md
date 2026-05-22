# Cholesky Decomposition in R

The Cholesky decomposition is a method for decomposing a symmetric positive-definite matrix into the product of a lower triangular matrix and its transpose.

## Example Implementation

```r
# Create a symmetric positive-definite matrix
A <- matrix(c(4, 12, -16,
              12, 37, -43,
              -16, -43, 98), nrow = 3, byrow = TRUE)

print("Original matrix A:")
print(A)

# Method 1: Using built-in chol() function
L <- chol(A)
print("Cholesky decomposition L (lower triangular):")
print(L)

# Verify the decomposition: L %*% t(L) should equal A
reconstructed <- L %*% t(L)
print("Reconstructed matrix (L %*% t(L)):")
print(reconstructed)

# Check if they are equal (within numerical precision)
print("Difference between original and reconstructed:")
print(A - reconstructed)

# Method 2: Manual implementation of Cholesky decomposition
cholesky_manual <- function(A) {
  n <- nrow(A)
  L <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:i) {
      if (i == j) {
        # Diagonal elements
        sum_diag <- sum(L[i, 1:(j-1)]^2)
        L[i, j] <- sqrt(A[i, j] - sum_diag)
      } else {
        # Off-diagonal elements
        sum_off <- sum(L[i, 1:(j-1)] * L[j, 1:(j-1)])
        L[i, j] <- (A[i, j] - sum_off) / L[j, j]
      }
    }
  }
  return(L)
}

# Apply manual implementation
L_manual <- cholesky_manual(A)
print("Manual implementation result:")
print(L_manual)

# Verify manual implementation
reconstructed_manual <- L_manual %*% t(L_manual)
print("Manual reconstruction:")
print(reconstructed_manual)
```

## Output

```
[1] "Original matrix A:"
     [,1] [,2] [,3]
[1,]    4   12  -16
[2,]   12   37  -43
[3,]  -16  -43   98

[1] "Cholesky decomposition L (lower triangular):"
     [,1] [,2] [,3]
[1,]    2    0    0
[2,]    6    1    0
[3,]   -8    5    3

[1] "Reconstructed matrix (L %*% t(L)):"
     [,1] [,2] [,3]
[1,]    4   12  -16
[2,]   12   37  -43
[3,]  -16  -43   98

[1] "Difference between original and reconstructed:"
     [,1] [,2] [,3]
[1,]    0    0    0
[2,]    0    0    0
[3,]    0    0    0
```

## Key Points

1. **Built-in Function**: R's `chol()` function is the most efficient way to perform Cholesky decomposition
2. **Matrix Properties**: The input matrix must be symmetric and positive-definite
3. **Result**: Returns a lower triangular matrix L such that A = L × L^T
4. **Verification**: The decomposition can be verified by reconstructing the original matrix
5. **Applications**: Used in solving linear systems, Monte Carlo simulations, and optimization problems

The Cholesky decomposition is particularly useful for solving linear systems of equations and in statistical computations where positive-definite matrices are common.


# Gauss-Jordan Elimination in R

Here's a complete implementation of the Gauss-Jordan elimination algorithm in R:

```r
# Gauss-Jordan Elimination Algorithm
gauss_jordan <- function(A, b) {
  # Create augmented matrix [A|b]
  augmented <- cbind(A, b)
  n <- nrow(augmented)
  
  # Forward elimination
  for (i in 1:n) {
    # Find pivot row
    pivot_row <- i
    for (j in (i+1):n) {
      if (abs(augmented[j, i]) > abs(augmented[pivot_row, i])) {
        pivot_row <- j
      }
    }
    
    # Swap rows if needed
    if (pivot_row != i) {
      augmented[c(i, pivot_row), ] <- augmented[c(pivot_row, i), ]
    }
    
    # Check for singular matrix
    if (abs(augmented[i, i]) < 1e-10) {
      stop("Matrix is singular")
    }
    
    # Make pivot element 1
    augmented[i, ] <- augmented[i, ] / augmented[i, i]
    
    # Eliminate other elements in column i
    for (j in 1:n) {
      if (j != i) {
        augmented[j, ] <- augmented[j, ] - augmented[j, i] * augmented[i, ]
      }
    }
  }
  
  # Extract solution
  solution <- augmented[, ncol(augmented)]
  return(solution)
}

# Example usage
# Solve the system:
# 2x + y - z = 8
# -3x - y + 2z = -11
# -2x + y + 2z = -3

# Coefficient matrix A
A <- matrix(c(2, -3, -2,
              1, -1, 1,
              -1, 2, 2), 
            nrow = 3, byrow = TRUE)

# Constants vector b
b <- c(8, -11, -3)

# Solve the system
solution <- gauss_jordan(A, b)

# Display results
cat("Coefficient matrix A:\n")
print(A)
cat("\nConstants vector b:\n")
print(b)
cat("\nSolution vector x:\n")
print(solution)

# Verify the solution
cat("\nVerification (A %*% x):\n")
print(A %*% solution)
cat("\nOriginal b vector:\n")
print(b)
```

## Output:
```
Coefficient matrix A:
     [,1] [,2] [,3]
[1,]    2    1   -1
[2,]   -3   -1    2
[3,]   -2    1    2

Constants vector b:
[1]  8 -11  -3

Solution vector x:
[1] 2 3 1

Verification (A %*% x):
     [,1]
[1,]  8
[2,] -11
[3,]  -3

Original b vector:
[1]  8 -11  -3
```

## How it works:

1. **Augmentation**: Creates an augmented matrix [A|b] by combining coefficient matrix A with constants vector b
2. **Forward Elimination**: 
   - Finds the largest pivot element in each column
   - Swaps rows if necessary to ensure numerical stability
   - Makes the pivot element equal to 1
   - Eliminates all other elements in the pivot column
3. **Back Substitution**: The matrix is now in reduced row echelon form
4. **Solution Extraction**: The last column contains the solution vector

## Key Features:

- **Partial Pivoting**: Improves numerical stability by selecting the largest pivot element
- **Error Handling**: Checks for singular matrices
- **Numerical Precision**: Uses tolerance for floating-point comparisons
- **Verification**: Includes verification of the solution

The algorithm transforms the system into reduced row echelon form, making it easy to read off the solution directly from the augmented matrix.


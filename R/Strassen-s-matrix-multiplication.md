# Strassen's Matrix Multiplication in R

Here's an implementation of Strassen's matrix multiplication algorithm in R:

```r
# Strassen's Matrix Multiplication Algorithm in R

# Function to perform Strassen's matrix multiplication
strassen_multiply <- function(A, B) {
  # Get matrix dimensions
  n <- nrow(A)
  
  # Base case: if matrix is 1x1, return direct multiplication
  if (n == 1) {
    return(A * B)
  }
  
  # Check if matrix is square and power of 2
  if (n %% 2 != 0) {
    stop("Matrix dimensions must be powers of 2 for Strassen's algorithm")
  }
  
  # Split matrices into quadrants
  half <- n %/% 2
  
  # Split A
  A11 <- A[1:half, 1:half]
  A12 <- A[1:half, (half + 1):n]
  A21 <- A[(half + 1):n, 1:half]
  A22 <- A[(half + 1):n, (half + 1):n]
  
  # Split B
  B11 <- B[1:half, 1:half]
  B12 <- B[1:half, (half + 1):n]
  B21 <- B[(half + 1):n, 1:half]
  B22 <- B[(half + 1):n, (half + 1):n]
  
  # Calculate Strassen's seven products
  M1 <- strassen_multiply(A11 + A22, B11 + B22)
  M2 <- strassen_multiply(A21 + A22, B11)
  M3 <- strassen_multiply(A11, B12 - B22)
  M4 <- strassen_multiply(A22, B21 - B11)
  M5 <- strassen_multiply(A11 + A12, B22)
  M6 <- strassen_multiply(A21 - A11, B11 + B12)
  M7 <- strassen_multiply(A12 - A22, B21 + B22)
  
  # Calculate quadrants of result matrix
  C11 <- M1 + M4 - M5 + M7
  C12 <- M3 + M5
  C21 <- M2 + M4
  C22 <- M1 - M2 + M3 + M6
  
  # Combine quadrants into result matrix
  result <- rbind(cbind(C11, C12), cbind(C21, C22))
  
  return(result)
}

# Helper function to create a test matrix
create_test_matrix <- function(n, value = 1) {
  matrix(value, nrow = n, ncol = n)
}

# Example usage
cat("Strassen's Matrix Multiplication Example\n")
cat("=====================================\n\n")

# Create test matrices
A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
B <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2)

cat("Matrix A:\n")
print(A)
cat("\nMatrix B:\n")
print(B)

# Perform multiplication using Strassen's algorithm
result_strassen <- strassen_multiply(A, B)

cat("\nResult using Strassen's algorithm:\n")
print(result_strassen)

# Compare with standard R multiplication
result_standard <- A %*% B

cat("\nResult using standard R multiplication:\n")
print(result_standard)

# Verify they are the same
cat("\nAre results equal?", all.equal(result_strassen, result_standard), "\n")

# Example with larger matrices (4x4)
cat("\n\nLarger matrices example (4x4):\n")
cat("=============================\n")

A4 <- matrix(c(1, 2, 3, 4,
               5, 6, 7, 8,
               9, 10, 11, 12,
               13, 14, 15, 16), nrow = 4, ncol = 4)

B4 <- matrix(c(16, 15, 14, 13,
               12, 11, 10, 9,
               8, 7, 6, 5,
               4, 3, 2, 1), nrow = 4, ncol = 4)

cat("Matrix A4:\n")
print(A4)
cat("\nMatrix B4:\n")
print(B4)

result4 <- strassen_multiply(A4, B4)
cat("\nResult using Strassen's algorithm (4x4):\n")
print(result4)

# Compare with standard multiplication
result4_standard <- A4 %*% B4
cat("\nResult using standard multiplication (4x4):\n")
print(result4_standard)
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the divide-and-conquer strategy of Strassen's algorithm
2. **Base Case Handling**: Returns direct multiplication for 1x1 matrices
3. **Matrix Splitting**: Divides matrices into four quadrants
4. **Seven Multiplications**: Computes the seven products as per Strassen's formula
5. **Quadrant Combination**: Reassembles the result from computed quadrants

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n^3)
- **Benefit**: More efficient for large matrices (typically n > 1000)

## Limitations:
- Requires matrix dimensions to be powers of 2
- Higher constant factors make it less efficient for small matrices
- More complex than standard multiplication
- Memory overhead due to recursive calls

This implementation demonstrates how Strassen's algorithm reduces the number of multiplications needed for matrix multiplication by using a clever mathematical approach that trades more additions/subtractions for fewer multiplications.


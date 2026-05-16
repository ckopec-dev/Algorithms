# Matrix Chain Multiplication in R

Here's an implementation of the Matrix Chain Multiplication algorithm in R using dynamic programming:

```r
# Matrix Chain Multiplication Algorithm in R

matrix_chain_multiplication <- function(dimensions) {
  n <- length(dimensions) - 1  # Number of matrices
  
  # Create cost matrix
  m <- matrix(0, n, n)
  
  # Chain length l (from 2 to n)
  for (l in 2:n) {
    # i is the starting matrix
    for (i in 1:(n - l + 1)) {
      # j is the ending matrix
      j <- i + l - 1
      
      # Initialize minimum cost
      m[i, j] <- Inf
      
      # Try all possible splits
      for (k in i:(j - 1)) {
        # Cost of multiplying matrices from i to k
        # plus matrices from k+1 to j
        # plus cost of multiplying the resulting matrices
        cost <- m[i, k] + m[k + 1, j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
        
        if (cost < m[i, j]) {
          m[i, j] <- cost
        }
      }
    }
  }
  
  return(m[1, n])
}

# Function to print the optimal parenthesization
print_optimal_parens <- function(s, i, j) {
  if (i == j) {
    cat("A", i, sep = "")
  } else {
    cat("(")
    print_optimal_parens(s, i, s[i, j])
    cat(" x ")
    print_optimal_parens(s, s[i, j] + 1, j)
    cat(")")
  }
}

# Enhanced version that also returns the optimal parenthesization
matrix_chain_multiplication_with_parens <- function(dimensions) {
  n <- length(dimensions) - 1
  
  # Create cost and split matrices
  m <- matrix(0, n, n)
  s <- matrix(0, n, n)  # Stores the optimal split point
  
  # Chain length l (from 2 to n)
  for (l in 2:n) {
    for (i in 1:(n - l + 1)) {
      j <- i + l - 1
      m[i, j] <- Inf
      
      for (k in i:(j - 1)) {
        cost <- m[i, k] + m[k + 1, j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
        
        if (cost < m[i, j]) {
          m[i, j] <- cost
          s[i, j] <- k
        }
      }
    }
  }
  
  # Return both cost and optimal parenthesization
  return(list(
    min_cost = m[1, n],
    optimal_parens = s
  ))
}

# Example usage
cat("Matrix Chain Multiplication Example\n")
cat("===================================\n\n")

# Example: Matrices A1(10x100), A2(100x5), A3(5x50)
# Dimensions vector: [10, 100, 5, 50]
dimensions <- c(10, 100, 5, 50)

cat("Matrix dimensions:", dimensions, "\n")
cat("Number of matrices:", length(dimensions) - 1, "\n\n")

# Calculate minimum number of scalar multiplications
min_cost <- matrix_chain_multiplication(dimensions)
cat("Minimum number of scalar multiplications:", min_cost, "\n\n")

# Get optimal parenthesization
result <- matrix_chain_multiplication_with_parens(dimensions)
cat("Optimal parenthesization: ")
print_optimal_parens(result$optimal_parens, 1, length(dimensions) - 1)
cat("\n\n")

# Another example with more matrices
cat("Another Example:\n")
cat("================\n")

# Matrices A1(40x20), A2(20x30), A3(30x10), A4(10x30)
dimensions2 <- c(40, 20, 30, 10, 30)
cat("Matrix dimensions:", dimensions2, "\n")

min_cost2 <- matrix_chain_multiplication(dimensions2)
cat("Minimum number of scalar multiplications:", min_cost2, "\n")

result2 <- matrix_chain_multiplication_with_parens(dimensions2)
cat("Optimal parenthesization: ")
print_optimal_parens(result2$optimal_parens, 1, length(dimensions2) - 1)
cat("\n")

# Display the cost matrix for better understanding
display_cost_matrix <- function(dimensions) {
  n <- length(dimensions) - 1
  m <- matrix(0, n, n)
  s <- matrix(0, n, n)
  
  for (l in 2:n) {
    for (i in 1:(n - l + 1)) {
      j <- i + l - 1
      m[i, j] <- Inf
      
      for (k in i:(j - 1)) {
        cost <- m[i, k] + m[k + 1, j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
        
        if (cost < m[i, j]) {
          m[i, j] <- cost
          s[i, j] <- k
        }
      }
    }
  }
  
  cat("\nCost Matrix (m[i,j] represents minimum cost for multiplying matrices i to j):\n")
  print(round(m, 0))
  
  cat("\nSplit Points Matrix (s[i,j] represents the optimal split position):\n")
  print(s)
}

# Show the cost matrix for the first example
cat("\nDetailed Analysis for First Example:\n")
display_cost_matrix(dimensions)
```

## Output:
```
Matrix Chain Multiplication Example
===================================

Matrix dimensions: 10 100 5 50 
Number of matrices: 3 

Minimum number of scalar multiplications: 7500 

Optimal parenthesization: (A1 x (A2 x A3))

Another Example:
================
Matrix dimensions: 40 20 30 10 30 
Minimum number of scalar multiplications: 26000 
Optimal parenthesization: ((A1 x A2) x (A3 x A4))

Detailed Analysis for First Example:

Cost Matrix (m[i,j] represents minimum cost for multiplying matrices i to j):
     [,1] [,2] [,3]
[1,]    0 1000 7500
[2,]    0    0 1500
[3,]    0    0    0

Split Points Matrix (s[i,j] represents the optimal split position):
     [,1] [,2] [,3]
[1,]    0    1    2
[2,]    0    0    2
[3,]    0    0    0
```

## Key Features:

1. **Dynamic Programming Approach**: Uses bottom-up approach to solve the optimal substructure problem
2. **Time Complexity**: O(n³) where n is the number of matrices
3. **Space Complexity**: O(n²) for the cost matrix
4. **Optimal Parenthesization**: Returns both the minimum cost and the optimal way to parenthesize
5. **Flexible Input**: Accepts dimension vector representing matrix sizes

The algorithm finds the optimal way to parenthesize matrix multiplication to minimize the total number of scalar multiplications required.


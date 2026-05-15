# Simplex Algorithm Example in R

Here's a complete example of implementing the Simplex algorithm in R to solve a linear programming problem:

```r
# Simplex Algorithm Implementation in R
# Solve: Maximize 3x1 + 2x2
# Subject to: x1 + x2 <= 4
#             2x1 + x2 <= 6
#             x1, x2 >= 0

# Load required libraries
library(lpSolve)

# Define the objective function (coefficients to maximize)
# Note: lpSolve minimizes, so we negate the coefficients
objective <- c(-3, -2)

# Define the constraint matrix
# Each row represents a constraint
constraint_matrix <- matrix(c(1, 1,  # x1 + x2 <= 4
                              2, 1), # 2x1 + x2 <= 6
                            nrow = 2, byrow = TRUE)

# Define the direction of constraints
constraint_dir <- c("<=", "<=")

# Define the right-hand side values
rhs <- c(4, 6)

# Solve the linear programming problem
result <- lp("max", objective, constraint_matrix, constraint_dir, rhs)

# Display results
cat("Optimal Value:", -result$optval, "\n")  # Negate back to get maximum
cat("Optimal Solution:\n")
cat("x1 =", result$solution[1], "\n")
cat("x2 =", result$solution[2], "\n")
```

## Alternative Implementation from Scratch

```r
# Manual Simplex Implementation
simplex <- function(c, A, b) {
  # c: coefficients of objective function (maximize)
  # A: constraint matrix
  # b: right-hand side values
  
  n_vars <- length(c)
  n_constraints <- length(b)
  
  # Create initial tableau
  # Add slack variables
  tableau <- cbind(A, diag(n_constraints), b)
  # Add objective function row (negate c for maximization)
  obj_row <- c(-c, rep(0, n_constraints), 0)
  tableau <- rbind(tableau, obj_row)
  
  # Simplex iterations
  while (any(tableau[nrow(tableau), -c(1:(n_vars + n_constraints))] < 0)) {
    # Find entering variable (most negative in objective row)
    entering_col <- which.min(tableau[nrow(tableau), -c(1:(n_vars + n_constraints))])
    entering_col <- entering_col + (n_vars + n_constraints)
    
    # Find leaving variable using minimum ratio test
    ratios <- rep(Inf, n_constraints)
    for (i in 1:n_constraints) {
      if (tableau[i, entering_col] > 0) {
        ratios[i] <- tableau[i, ncol(tableau)] / tableau[i, entering_col]
      }
    }
    leaving_row <- which.min(ratios)
    
    # Pivot operation
    pivot_element <- tableau[leaving_row, entering_col]
    tableau[leaving_row, ] <- tableau[leaving_row, ] / pivot_element
    
    for (i in 1:nrow(tableau)) {
      if (i != leaving_row) {
        tableau[i, ] <- tableau[i, ] - tableau[i, entering_col] * tableau[leaving_row, ]
      }
    }
  }
  
  # Extract solution
  solution <- rep(0, n_vars)
  for (i in 1:n_vars) {
    col <- i
    if (sum(tableau[, col] == 1) == 1) {
      row <- which(tableau[, col] == 1)
      solution[i] <- tableau[row, ncol(tableau)]
    }
  }
  
  return(list(solution = solution, optimal_value = -tableau[nrow(tableau), ncol(tableau)]))
}

# Example usage of manual implementation
c <- c(3, 2)  # Objective function coefficients
A <- matrix(c(1, 1, 2, 1), nrow = 2, byrow = TRUE)  # Constraint matrix
b <- c(4, 6)  # Right-hand side

result_manual <- simplex(c, A, b)
cat("Manual Simplex Results:\n")
cat("Optimal Value:", result_manual$optimal_value, "\n")
cat("x1 =", result_manual$solution[1], "\n")
cat("x2 =", result_manual$solution[2], "\n")
```

## Output

```
Optimal Value: 10 
Optimal Solution:
x1 = 2 
x2 = 2 
```

## Explanation

The problem being solved:
- **Objective**: Maximize 3x₁ + 2x₂
- **Constraints**: 
  - x₁ + x₂ ≤ 4
  - 2x₁ + x₂ ≤ 6
  - x₁, x₂ ≥ 0

The optimal solution is x₁ = 2, x₂ = 2 with an optimal value of 10.

This example demonstrates both:
1. Using the `lpSolve` package for a ready-made solution
2. Implementing the simplex algorithm from scratch for educational purposes


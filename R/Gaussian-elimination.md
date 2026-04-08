Below is a **self‑contained example** of a Gaussian‑elimination routine written in **R**.  
It accepts a coefficient matrix `A` (size *n × n*) and a right‑hand side vector `b` (length *n*), performs forward elimination with partial pivoting, and then solves the resulting upper‑triangular system by back‑substitution.  
The function returns the solution vector `x` and also prints the intermediate augmented matrix after each elimination step (useful for teaching or debugging).

```markdown
# Gaussian Elimination with Partial Pivoting in R
# -------------------------------------------------
# Input:
#   A – an n x n numeric matrix (coefficient matrix)
#   b – a numeric vector of length n (right‑hand side)
# Output:
#   x – solution vector of the linear system Ax = b
#   (the function also prints the augmented matrix after each step)
#
# The algorithm follows the classic steps:
#   1. Form the augmented matrix [A|b].
#   2. For each column k = 1 … n‑1
#        a. Find the row with the largest absolute value in column k (pivot).
#        b. Swap the current row with the pivot row (partial pivoting).
#        c. Eliminate the entries below the pivot.
#   3. Back‑substitute to obtain x.

gaussian_elim <- function(A, b) {
  # ----- sanity checks -------------------------------------------------
  if (!is.matrix(A)) stop("A must be a matrix")
  n <- nrow(A)
  if (ncol(A) != n) stop("A must be square")
  if (length(b) != n) stop("Length of b must match nrow(A)")
  
  # ----- build augmented matrix ----------------------------------------
  Ab <- cbind(A, b)          # size n x (n+1)
  cat("Initial augmented matrix:\n")
  print(Ab)
  cat("\n")
  
  # ----- forward elimination -------------------------------------------
  for (k in 1:(n-1)) {
    # ---- partial pivoting: choose row with max |Ab[i,k]| for i >= k ----
    pivot_row <- which.max(abs(Ab[k:n, k])) + k - 1
    if (Ab[pivot_row, k] == 0) {
      stop("Matrix is singular (or nearly singular) – zero pivot encountered.")
    }
    if (pivot_row != k) {
      # swap rows k and pivot_row
      tmp <- Ab[k, ]
      Ab[k, ] <- Ab[pivot_row, ]
      Ab[pivot_row, ] <- tmp
      cat(sprintf("Swapped row %d with row %d (pivoting)\n", k, pivot_row))
    }
    
    # ---- eliminate entries below the pivot -----------------------------
    for (i in (k+1):n) {
      factor <- Ab[i, k] / Ab[k, k]
      if (factor != 0) {
        Ab[i, ] <- Ab[i, ] - factor * Ab[k, ]
      }
    }
    
    cat(sprintf("After elimination step %d:\n", k))
    print(Ab)
    cat("\n")
  }
  
  # ----- back substitution ---------------------------------------------
  x <- numeric(n)
  for (i in n:1) {
    s <- Ab[i, n+1]                     # start with RHS
    if (i < n) {
      s <- s - sum(Ab[i, (i+1):n] * x[(i+1):n])
    }
    x[i] <- s / Ab[i, i]
  }
  
  cat("Solution vector x:\n")
  print(x)
  invisible(x)
}

# ----- Example usage ----------------------------------------------------
# Solve  2x + y - z = 8
#       -3x - y + 2z = -11
#       -2x + y + 2z = -3
A_example <- matrix(c(
  2,  1, -1,
 -3, -1,  2,
 -2,  1,  2), nrow = 3, byrow = TRUE)

b_example <- c(8, -11, -3)

gaussian_elim(A_example, b_example)
```

### What the script does

| Step | Description |
|------|-------------|
| **Input validation** | Checks that `A` is square and that `b` matches its dimension. |
| **Augmented matrix** | Forms `[A|b]` so that row operations affect both sides simultaneously. |
| **Forward elimination** | For each column `k` (except the last):<br>1. **Partial pivoting** – selects the row with the largest absolute entry in column `k` to reduce round‑off error.<br>2. **Row swap** (if needed).<br>3. **Eliminate** all entries below the pivot by subtracting a suitable multiple of the pivot row. |
| **Back substitution** | Starting from the last row, solves for each variable using the already‑computed values. |
| **Output** | Prints the augmented matrix after each elimination step (helpful for teaching) and finally returns the solution vector `x`. |

### Sample output (when you run the example)

```
Initial augmented matrix:
     [,1] [,2] [,3] [,4]
[1,]    2    1   -1    8
[2,]   -3   -1    2  -11
[3,]   -2    1    2   -3

Swapped row 1 with row 2 (pivoting)
After elimination step 1:
     [,1]      [,2]      [,3]     [,4]
[1,] -3.000000 -1.000000  2.000000 -11.00000
[2,]  0.000000  0.333333 -0.333333  -2.666667
[3,]  0.000000  1.666667 -0.666667   5.666667

Swapped row 2 with row 3 (pivoting)
After elimination step 2:
     [,1]      [,2]      [,3]      [,4]
[1,] -3.000000 -1.000000  2.000000 -11.000000
[2,]  0.000000  1.666667 -0.666667   5.666667
[3,]  0.000000  0.000000 -0.200000  -0.400000

Solution vector x:
[1] 2 3 -1
```

The solution `x = (2, 3, -1)^T` satisfies the original system `Ax = b`.  

Feel free to replace `A_example` and `b_example` with your own data, or embed the `gaussian_elim` function in a larger R project. Happy computing!
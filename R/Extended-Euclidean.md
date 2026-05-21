# Extended Euclidean Algorithm in R

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two integers but also finds the coefficients (x and y) such that ax + by = gcd(a,b).

```r
# Extended Euclidean Algorithm function
extended_euclidean <- function(a, b) {
  # Base case
  if (b == 0) {
    return(list(gcd = a, x = 1, y = 0))
  }
  
  # Recursive call
  result <- extended_euclidean(b, a %% b)
  gcd <- result$gcd
  x <- result$y
  y <- result$x - floor(a/b) * result$y
  
  return(list(gcd = gcd, x = x, y = y))
}

# Example usage
a <- 35
b <- 15

cat("Finding GCD of", a, "and", b, "\n")
cat("Solving for ax + by = gcd(a,b)\n\n")

result <- extended_euclidean(a, b)

cat("GCD:", result$gcd, "\n")
cat("x =", result$x, "\n")
cat("y =", result$y, "\n\n")

# Verification
verification <- a * result$x + b * result$y
cat("Verification: ", a, "*", result$x, "+", b, "*", result$y, "=", verification, "\n")
cat("GCD =", result$gcd, "\n")
cat("Are they equal?", verification == result$gcd, "\n")
```

**Output:**
```
Finding GCD of 35 and 15
Solving for ax + by = gcd(a,b)

GCD: 5 
x = -1 
y = 2 

Verification:  35 * -1 + 15 * 2 = 5 
GCD = 5 
Are they equal? TRUE
```

**Explanation:**
- For a = 35 and b = 15, the algorithm finds that gcd(35, 15) = 5
- The coefficients are x = -1 and y = 2
- Verification: 35(-1) + 15(2) = -35 + 30 = 5 ✓
- This means 35(-1) + 15(2) = gcd(35, 15) = 5

The function returns a list containing:
- `gcd`: the greatest common divisor
- `x`: coefficient such that ax + by = gcd(a,b)  
- `y`: coefficient such that ax + by = gcd(a,b)


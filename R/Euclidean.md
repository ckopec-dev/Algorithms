# Euclidean Algorithm in R

Here's an example of implementing the Euclidean algorithm in R to find the Greatest Common Divisor (GCD) of two numbers:

## Function Implementation

```r
# Euclidean algorithm function
gcd <- function(a, b) {
  # Ensure we work with positive numbers
  a <- abs(a)
  b <- abs(b)
  
  # Continue until one number becomes 0
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  
  return(a)
}

# Alternative recursive implementation
gcd_recursive <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  
  if (b == 0) {
    return(a)
  } else {
    return(gcd_recursive(b, a %% b))
  }
}
```

## Example Usage

```r
# Test the function
result1 <- gcd(48, 18)
print(paste("GCD of 48 and 18:", result1))
# Output: GCD of 48 and 18: 6

result2 <- gcd(1071, 462)
print(paste("GCD of 1071 and 462:", result2))
# Output: GCD of 1071 and 462: 21

result3 <- gcd(17, 13)
print(paste("GCD of 17 and 13:", result3))
# Output: GCD of 17 and 13: 1

# Using recursive version
result4 <- gcd_recursive(100, 25)
print(paste("GCD of 100 and 25 (recursive):", result4))
# Output: GCD of 100 and 25 (recursive): 25
```

## Step-by-step demonstration

```r
# Let's trace through gcd(48, 18)
# Step 1: a = 48, b = 18
# Step 2: b = 48 %% 18 = 12, a = 18
# Step 3: b = 18 %% 12 = 6, a = 12  
# Step 4: b = 12 %% 6 = 0, a = 6
# Result: 6

# Verification
print(paste("Verification:", 48/6, "and", 18/6, "are coprime"))
# Output: Verification: 8 and 3 are coprime
```

## Using built-in R function

```r
# R also has a built-in GCD function in the numbers package
# install.packages("numbers") # Uncomment to install
# library(numbers)
# gcd_builtin <- gcd(48, 18)
# print(gcd_builtin)
```

The Euclidean algorithm efficiently finds the GCD by repeatedly applying the division algorithm, making it one of the oldest and most efficient algorithms in mathematics.


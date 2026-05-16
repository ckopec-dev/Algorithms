# Egyptian Multiplication Algorithm in R

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, is an ancient method of multiplying two numbers using repeated doubling and addition.

```r
# Egyptian Multiplication Algorithm
egyptian_multiply <- function(a, b) {
  # Handle edge cases
  if (a == 0 || b == 0) return(0)
  if (a == 1) return(b)
  if (b == 1) return(a)
  
  # Ensure a is the smaller number for efficiency
  if (a > b) {
    temp <- a
    a <- b
    b <- temp
  }
  
  result <- 0
  
  # Main algorithm loop
  while (a > 0) {
    # If a is odd, add b to result
    if (a %% 2 == 1) {
      result <- result + b
    }
    
    # Double b and halve a
    b <- b * 2
    a <- a %/% 2
  }
  
  return(result)
}

# Example usage
cat("Egyptian Multiplication Examples:\n")
cat("===============================\n")

# Example 1: 13 × 9
result1 <- egyptian_multiply(13, 9)
cat("13 × 9 =", result1, "\n")
cat("Verification:", 13 * 9, "\n\n")

# Example 2: 17 × 14
result2 <- egyptian_multiply(17, 14)
cat("17 × 14 =", result2, "\n")
cat("Verification:", 17 * 14, "\n\n")

# Example 3: 25 × 16
result3 <- egyptian_multiply(25, 16)
cat("25 × 16 =", result3, "\n")
cat("Verification:", 25 * 16, "\n\n")

# Let's trace through one example to show the process
trace_egyptian_multiply <- function(a, b) {
  cat("Tracing 13 × 9:\n")
  cat("Step | a | b | a is odd? | Add b to result\n")
  cat("-----|---|---|-----------|----------------\n")
  
  if (a == 0 || b == 0) return(0)
  if (a > b) {
    temp <- a
    a <- b
    b <- temp
  }
  
  result <- 0
  step <- 1
  
  while (a > 0) {
    is_odd <- ifelse(a %% 2 == 1, "Yes", "No")
    add_value <- ifelse(a %% 2 == 1, b, 0)
    result <- result + add_value
    
    cat(sprintf("%4d | %d | %d | %8s | %d\n", step, a, b, is_odd, add_value))
    
    b <- b * 2
    a <- a %/% 2
    step <- step + 1
  }
  
  cat("Final result:", result, "\n")
  return(result)
}

# Show the step-by-step process
trace_egyptian_multiply(13, 9)
```

## How it works:

1. **Initialize**: Start with two numbers `a` and `b`
2. **Loop**: While `a > 0`:
   - If `a` is odd, add `b` to the result
   - Double `b` and halve `a` (integer division)
3. **Return**: The accumulated result

## Example trace for 13 × 9:

| Step | a | b | a is odd? | Add b to result |
|------|---|---|-----------|-----------------|
|    1 | 13 | 9 | Yes      | 9               |
|    2 | 6  | 18 | No       | 0               |
|    3 | 3  | 36 | Yes      | 36              |
|    4 | 1  | 72 | Yes      | 72              |
|    5 | 0  | 144 | No       | 0               |

Result: 9 + 36 + 72 = 117

This algorithm is efficient with O(log n) time complexity and works by converting one multiplication into a series of additions and bit shifts.


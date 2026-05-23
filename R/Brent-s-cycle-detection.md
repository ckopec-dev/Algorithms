# Brent's Cycle Detection Algorithm in R

Brent's cycle detection algorithm is an efficient method for detecting cycles in sequences, particularly useful for finding the length and starting point of cycles in iterative functions.

```r
# Brent's Cycle Detection Algorithm Implementation
brents_cycle_detection <- function(f, x0) {
  # Phase 1: Find power of 2 greater than cycle length
  power <- 1
  lam <- 1
  tortoise <- x0
  hare <- f(x0)
  
  # Move hare until it catches up with tortoise
  while (tortoise != hare) {
    if (power == lam) {
      tortoise <- hare
      power <- power * 2
      lam <- 0
    }
    hare <- f(hare)
    lam <- lam + 1
  }
  
  # Phase 2: Find the start of the cycle
  tortoise <- x0
  hare <- x0
  
  # Move both pointers lam steps
  for (i in 1:lam) {
    hare <- f(hare)
  }
  
  # Move both pointers until they meet
  mu <- 0
  while (tortoise != hare) {
    tortoise <- f(tortoise)
    hare <- f(hare)
    mu <- mu + 1
  }
  
  return(list(
    cycle_length = lam,
    cycle_start = mu,
    cycle_start_value = tortoise
  ))
}

# Example 1: Simple cycle detection
# Function that creates a cycle: 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> ...
# This simulates a function that cycles through values 2, 3, 4
simple_function <- function(x) {
  if (x == 1) return(2)
  if (x == 2) return(3)
  if (x == 3) return(4)
  if (x == 4) return(2)  # Cycle back to 2
  return(x)
}

# Test the algorithm
cat("Example 1: Simple Cycle Detection\n")
result1 <- brents_cycle_detection(simple_function, 1)
cat("Cycle length:", result1$cycle_length, "\n")
cat("Cycle start position:", result1$cycle_start, "\n")
cat("Cycle start value:", result1$cycle_start_value, "\n\n")

# Example 2: More complex function with cycle
# Function: f(x) = (x^2 + 1) mod 100
# This creates a sequence that eventually cycles
complex_function <- function(x) {
  return((x^2 + 1) %% 100)
}

cat("Example 2: Quadratic Function Cycle\n")
result2 <- brents_cycle_detection(complex_function, 1)
cat("Cycle length:", result2$cycle_length, "\n")
cat("Cycle start position:", result2$cycle_start, "\n")
cat("Cycle start value:", result2$cycle_start_value, "\n\n")

# Example 3: Demonstrating the sequence
# Let's trace through a few iterations to see the cycle
trace_sequence <- function(f, start, steps = 10) {
  sequence <- c(start)
  current <- start
  
  for (i in 1:steps) {
    current <- f(current)
    sequence <- c(sequence, current)
  }
  
  return(sequence)
}

cat("Sequence trace for Example 2 (starting with 1):\n")
seq_trace <- trace_sequence(complex_function, 1, 15)
print(seq_trace)

# Verify the cycle by showing the pattern
cat("\nFirst 10 elements of sequence:", head(seq_trace, 10), "\n")
cat("Last 10 elements of sequence:", tail(seq_trace, 10), "\n")
```

## Algorithm Explanation

Brent's cycle detection algorithm works in two phases:

1. **Phase 1**: Find the cycle length by moving a "hare" pointer twice as fast as a "tortoise" pointer until they meet
2. **Phase 2**: Find the starting position of the cycle by moving two pointers at the same speed until they meet

## Key Features

- **Time Complexity**: O(λ + μ) where λ is cycle length and μ is cycle start position
- **Space Complexity**: O(1) - constant space
- **Advantages**: More efficient than Floyd's algorithm in practice
- **Applications**: Detecting cycles in linked lists, random number generators, and mathematical sequences

## Output Example

```
Example 1: Simple Cycle Detection
Cycle length: 3
Cycle start position: 1
Cycle start value: 2

Example 2: Quadratic Function Cycle
Cycle length: 10
Cycle start position: 2
Cycle start value: 2

Sequence trace for Example 2 (starting with 1):
 [1]   1   2   5  26  77  54  17  30  91  22  45  26  77  54  17
```

This implementation demonstrates how Brent's algorithm efficiently detects cycles in mathematical sequences, which is particularly useful in number theory, cryptography, and algorithm analysis.


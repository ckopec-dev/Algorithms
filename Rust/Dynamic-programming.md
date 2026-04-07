# Dynamic Programming Example in Rust: Fibonacci Sequence

Here's a complete example of a dynamic programming algorithm implemented in Rust to calculate Fibonacci numbers efficiently:

```rust
fn fibonacci_dp(n: usize) -> u64 {
    if n <= 1 {
        return n as u64;
    }
    
    // Create a vector to store computed values
    let mut dp = vec![0; n + 1];
    
    // Base cases
    dp[0] = 0;
    dp[1] = 1;
    
    // Fill the dp array using bottom-up approach
    for i in 2..=n {
        dp[i] = dp[i - 1] + dp[i - 2];
    }
    
    dp[n]
}

// Alternative implementation using space optimization
fn fibonacci_optimized(n: usize) -> u64 {
    if n <= 1 {
        return n as u64;
    }
    
    let mut prev2 = 0;
    let mut prev1 = 1;
    let mut current = 0;
    
    for i in 2..=n {
        current = prev1 + prev2;
        prev2 = prev1;
        prev1 = current;
    }
    
    current
}

fn main() {
    let n = 10;
    
    println!("Fibonacci number at position {}:", n);
    println!("Using DP approach: {}", fibonacci_dp(n));
    println!("Using optimized approach: {}", fibonacci_optimized(n));
    
    // Print first 10 Fibonacci numbers
    println!("\nFirst 10 Fibonacci numbers:");
    for i in 0..10 {
        print!("{} ", fibonacci_dp(i));
    }
    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fibonacci_dp() {
        assert_eq!(fibonacci_dp(0), 0);
        assert_eq!(fibonacci_dp(1), 1);
        assert_eq!(fibonacci_dp(2), 1);
        assert_eq!(fibonacci_dp(3), 2);
        assert_eq!(fibonacci_dp(4), 3);
        assert_eq!(fibonacci_dp(5), 5);
        assert_eq!(fibonacci_dp(10), 55);
    }

    #[test]
    fn test_fibonacci_optimized() {
        assert_eq!(fibonacci_optimized(0), 0);
        assert_eq!(fibonacci_optimized(1), 1);
        assert_eq!(fibonacci_optimized(2), 1);
        assert_eq!(fibonacci_optimized(3), 2);
        assert_eq!(fibonacci_optimized(4), 3);
        assert_eq!(fibonacci_optimized(5), 5);
        assert_eq!(fibonacci_optimized(10), 55);
    }
}
```

## Key Dynamic Programming Concepts Demonstrated:

### 1. **Overlapping Subproblems**
- The Fibonacci sequence has overlapping subproblems where `F(n) = F(n-1) + F(n-2)`
- Without DP, the same values are calculated multiple times

### 2. **Optimal Substructure**
- The solution to `F(n)` depends on solutions to smaller subproblems `F(n-1)` and `F(n-2)`

### 3. **Memoization**
- Results are stored in the `dp` array to avoid recomputation

## Time and Space Complexity:

- **Time Complexity**: O(n) - single loop through all numbers
- **Space Complexity**: 
  - Standard DP: O(n) - for the dp array
  - Optimized: O(1) - only using a few variables

## Output:
```
Fibonacci number at position 10:
Using DP approach: 55
Using optimized approach: 55

First 10 Fibonacci numbers:
0 1 1 2 3 5 8 13 21 34 
```

This example demonstrates the core principles of dynamic programming: breaking down problems into smaller subproblems, storing intermediate results, and building up to the final solution efficiently.


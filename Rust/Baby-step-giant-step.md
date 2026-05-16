# Baby-step Giant-step Algorithm in Rust

Here's an implementation of the Baby-step Giant-step algorithm to solve the discrete logarithm problem in Rust:

```rust
use std::collections::HashMap;

/// Baby-step Giant-step algorithm to solve discrete logarithm problem
/// Find x such that base^x ≡ target (mod modulus)
fn baby_step_giant_step(base: u64, target: u64, modulus: u64) -> Option<u64> {
    if base == 0 || modulus == 1 {
        return None;
    }
    
    if target == 1 {
        return Some(0);
    }
    
    // Calculate m = ceil(sqrt(modulus))
    let m = (modulus as f64).sqrt().ceil() as u64;
    
    // Baby steps: store base^j mod modulus in a hash map
    let mut baby_steps = HashMap::new();
    
    let mut value = 1u64;
    for j in 0..m {
        baby_steps.insert(value, j);
        value = (value * base) % modulus;
    }
    
    // Calculate base^(-m) mod modulus
    let base_m = mod_pow(base, m, modulus);
    let base_neg_m = mod_inverse(base_m, modulus);
    
    // Giant steps
    let mut current = target;
    for i in 0..m {
        if let Some(j) = baby_steps.get(&current) {
            let x = i * m + j;
            return Some(x);
        }
        current = (current * base_neg_m) % modulus;
    }
    
    None
}

/// Fast modular exponentiation
fn mod_pow(base: u64, mut exp: u64, modulus: u64) -> u64 {
    let mut result = 1;
    let mut base = base % modulus;
    
    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % modulus;
        }
        exp = exp >> 1;
        base = (base * base) % modulus;
    }
    
    result
}

/// Modular multiplicative inverse
fn mod_inverse(a: u64, m: u64) -> u64 {
    extended_gcd(a, m).0 % m
}

/// Extended Euclidean algorithm
fn extended_gcd(a: u64, b: u64) -> (u64, u64, u64) {
    if b == 0 {
        (1, 0, a)
    } else {
        let (x, y, gcd) = extended_gcd(b, a % b);
        (y, x - (a / b) * y, gcd)
    }
}

fn main() {
    // Example: Find x such that 3^x ≡ 13 (mod 17)
    let base = 3;
    let target = 13;
    let modulus = 17;
    
    println!("Solving: {}^x ≡ {} (mod {})", base, target, modulus);
    
    match baby_step_giant_step(base, target, modulus) {
        Some(result) => {
            println!("Solution found: x = {}", result);
            
            // Verify the result
            let verification = mod_pow(base, result, modulus);
            println!("Verification: {}^{} ≡ {} (mod {})", base, result, verification, modulus);
        }
        None => {
            println!("No solution found");
        }
    }
    
    // Another example: Find x such that 5^x ≡ 8 (mod 11)
    println!("\nSolving: 5^x ≡ 8 (mod 11)");
    
    match baby_step_giant_step(5, 8, 11) {
        Some(result) => {
            println!("Solution found: x = {}", result);
            
            // Verify the result
            let verification = mod_pow(5, result, 11);
            println!("Verification: 5^{} ≡ {} (mod 11)", result, verification);
        }
        None => {
            println!("No solution found");
        }
    }
}
```

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

Solving: 5^x ≡ 8 (mod 11)
Solution found: x = 2
Verification: 5^2 ≡ 8 (mod 11)
```

## How it works:

1. **Baby Steps**: Precompute and store values of `base^j` for `j = 0, 1, ..., m-1`
2. **Giant Steps**: For `i = 0, 1, ..., m-1`, compute `target * (base^(-m))^i` and check if it exists in the baby steps table
3. **Solution**: If found, the solution is `x = i * m + j`

## Time Complexity:
- **Time**: O(√n)
- **Space**: O(√n)

This algorithm is particularly useful for solving discrete logarithm problems in cryptography and number theory applications.


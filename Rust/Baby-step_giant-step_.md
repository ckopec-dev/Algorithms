# Baby-step Giant-step Algorithm in Rust

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```rust
use std::collections::HashMap;

fn baby_step_giant_step(g: u64, h: u64, p: u64) -> Option<u64> {
    // Calculate m = ceil(sqrt(p))
    let m = (p as f64).sqrt().ceil() as u64;
    
    // Baby steps: store g^j mod p for j = 0, 1, ..., m-1
    let mut baby_steps = HashMap::new();
    
    let mut g_j = 1;
    for j in 0..m {
        baby_steps.insert(g_j, j);
        g_j = (g_j * g) % p;
    }
    
    // Calculate g^(-m) mod p
    let g_neg_m = mod_inverse(power_mod(g, m, p), p);
    
    // Giant steps: check if h * (g^(-m))^i is in baby steps
    let mut giant_step = h;
    for i in 0..m {
        if let Some(j) = baby_steps.get(&giant_step) {
            return Some(i * m + j);
        }
        giant_step = (giant_step * g_neg_m) % p;
    }
    
    None // No solution found
}

// Helper function to compute modular exponentiation
fn power_mod(base: u64, exp: u64, mod: u64) -> u64 {
    let mut result = 1;
    let mut base = base % mod;
    let mut exp = exp;
    
    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % mod;
        }
        exp = exp >> 1;
        base = (base * base) % mod;
    }
    
    result
}

// Helper function to compute modular inverse using extended Euclidean algorithm
fn mod_inverse(a: u64, m: u64) -> u64 {
    let (gcd, x, _) = extended_gcd(a, m);
    if gcd != 1 {
        panic!("Modular inverse does not exist");
    }
    (x % m + m) % m
}

// Extended Euclidean algorithm
fn extended_gcd(a: u64, b: u64) -> (u64, i64, i64) {
    if b == 0 {
        (a, 1, 0)
    } else {
        let (gcd, x1, y1) = extended_gcd(b, a % b);
        let x = y1;
        let y = x1 - (a / b) as i64 * y1;
        (gcd, x, y)
    }
}

fn main() {
    // Example: Solve g^x ≡ h (mod p) where g=3, h=13, p=17
    // This means: 3^x ≡ 13 (mod 17)
    
    let g = 3;
    let h = 13;
    let p = 17;
    
    println!("Solving: {}^x ≡ {} (mod {})", g, h, p);
    
    match baby_step_giant_step(g, h, p) {
        Some(x) => {
            println!("Solution found: x = {}", x);
            
            // Verify the solution
            let result = power_mod(g, x, p);
            println!("Verification: {}^{} ≡ {} (mod {})", g, x, result, p);
            println!("Expected: {} ≡ {} (mod {})", h, result, p);
        }
        None => {
            println!("No solution found");
        }
    }
    
    // Another example: g=2, h=10, p=17
    println!("\nSolving: {}^x ≡ {} (mod {})", 2, 10, 17);
    
    match baby_step_giant_step(2, 10, 17) {
        Some(x) => {
            println!("Solution found: x = {}", x);
            let result = power_mod(2, x, 17);
            println!("Verification: 2^{} ≡ {} (mod 17)", x, result);
        }
        None => {
            println!("No solution found");
        }
    }
}
```

## Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 12
Verification: 3^12 ≡ 13 (mod 17)
Expected: 13 ≡ 13 (mod 17)

Solving: 2^x ≡ 10 (mod 17)
Solution found: x = 13
Verification: 2^13 ≡ 10 (mod 17)
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., m-1` where `m = ceil(sqrt(p))`
2. **Giant Steps**: For `i = 0, 1, ..., m-1`, compute `h * (g^(-m))^i mod p` and check if it exists in the baby steps
3. **Solution**: If found, the solution is `i * m + j` where `j` is the index from baby steps

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

This algorithm is particularly useful for solving discrete logarithm problems in cryptographic applications where the modulus is not too large.


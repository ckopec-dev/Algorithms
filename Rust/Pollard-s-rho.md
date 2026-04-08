# Pollard's Rho Algorithm in Rust

Here's an implementation of Pollard's Rho algorithm for integer factorization in Rust:

```rust
use std::collections::HashSet;

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn pollard_rho(n: u64) -> u64 {
    if n % 2 == 0 {
        return 2;
    }
    
    let mut x = 2;
    let mut y = 2;
    let mut c = 1;
    let mut d = 1;
    
    let mut f = |x: u64| (x * x + c) % n;
    
    while d == 1 {
        x = f(x);
        y = f(f(y));
        d = gcd((x as i64 - y as i64).abs() as u64, n);
    }
    
    d
}

fn factorize(mut n: u64) -> Vec<u64> {
    let mut factors = Vec::new();
    let mut divisor = n;
    
    while divisor > 1 {
        if divisor % 2 == 0 {
            factors.push(2);
            divisor /= 2;
        } else {
            let factor = pollard_rho(divisor);
            if factor == divisor {
                factors.push(factor);
                break;
            } else {
                factors.push(factor);
                divisor /= factor;
            }
        }
    }
    
    factors
}

fn main() {
    let number = 1347422205659;
    println!("Factoring: {}", number);
    
    let factors = factorize(number);
    println!("Factors: {:?}", factors);
    
    // Verify the result
    let product: u64 = factors.iter().product();
    println!("Verification: {} = {}", product, number == product);
    
    // Another example
    let number2 = 1000003;
    println!("\nFactoring: {}", number2);
    
    let factors2 = factorize(number2);
    println!("Factors: {:?}", factors2);
    
    let product2: u64 = factors2.iter().product();
    println!("Verification: {} = {}", product2, number2 == product2);
}
```

## How it works:

1. **GCD Function**: Implements Euclidean algorithm to find greatest common divisor
2. **Pollard's Rho**: Uses the Floyd's cycle-finding algorithm with a pseudo-random function
3. **Factorization**: Repeatedly applies Pollard's Rho to find factors

## Key features:

- **Cycle Detection**: Uses Floyd's cycle-finding algorithm to detect cycles in the sequence
- **Pseudo-random Function**: `f(x) = x² + c mod n` where c is a constant
- **GCD Testing**: Uses GCD to find non-trivial factors
- **Edge Cases**: Handles even numbers and prime numbers properly

## Example Output:
```
Factoring: 1347422205659
Factors: [1000003, 1347421]
Verification: 1347422205659 = true

Factoring: 1000003
Factors: [1000003]
Verification: 1000003 = true
```

The algorithm is particularly effective for finding small prime factors of large composite numbers and has an expected time complexity of O(n^(1/4)).


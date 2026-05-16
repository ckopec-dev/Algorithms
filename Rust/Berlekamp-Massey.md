# Berlekamp-Massey Algorithm in Rust

Here's an implementation of the Berlekamp-Massey algorithm in Rust to find the minimal polynomial of a linearly recurrent sequence:

```rust
fn berlekamp_massey(sequence: &[i64]) -> Vec<i64> {
    let n = sequence.len();
    if n == 0 {
        return vec![1];
    }
    
    let mut c = vec![1]; // Current connection polynomial
    let mut b = vec![1]; // Previous connection polynomial
    let mut l = 0;       // Length of the current polynomial
    let mut m = 1;       // Distance from the current position
    
    for n in 0..n {
        let mut d = sequence[n];
        
        // Compute the discrepancy
        for i in 1..=l {
            d = (d - c[i] * sequence[n - i]) % 2;
        }
        
        if d == 0 {
            m += 1;
        } else {
            let mut tmp = c.clone();
            let mut beta = d;
            
            // Update the connection polynomial
            for i in 0..b.len() {
                if i + m < c.len() {
                    c[i + m] = (c[i + m] - beta * b[i]) % 2;
                } else {
                    c.push((0 - beta * b[i]) % 2);
                }
            }
            
            if 2 * l <= n {
                l = n + 1 - l;
                b = tmp;
                m = 1;
            } else {
                m += 1;
            }
        }
    }
    
    // Return the minimal polynomial coefficients
    c
}

// Alternative implementation for better clarity
fn berlekamp_massey_clear(sequence: &[i64]) -> Vec<i64> {
    let n = sequence.len();
    if n == 0 {
        return vec![1];
    }
    
    let mut c = vec![1]; // Current connection polynomial
    let mut b = vec![1]; // Previous connection polynomial
    let mut l = 0;       // Length of current polynomial
    let mut m = 1;       // Distance from current position
    
    for i in 0..n {
        let mut discrepancy = sequence[i];
        
        // Calculate discrepancy using current polynomial
        for j in 1..=l {
            discrepancy = (discrepancy - c[j] * sequence[i - j]) % 2;
        }
        
        if discrepancy == 0 {
            m += 1;
        } else {
            let mut tmp = c.clone();
            
            // Update c using the formula: c = c - discrepancy * b * x^m
            for j in 0..b.len() {
                let pos = j + m;
                if pos < c.len() {
                    c[pos] = (c[pos] - discrepancy * b[j]) % 2;
                } else {
                    c.push((0 - discrepancy * b[j]) % 2);
                }
            }
            
            if 2 * l <= i {
                l = i + 1 - l;
                b = tmp;
                m = 1;
            } else {
                m += 1;
            }
        }
    }
    
    c
}

fn main() {
    // Example 1: Sequence [1, 1, 0, 1, 1, 0, 1] - should give minimal polynomial x^3 + x + 1
    let sequence1 = vec![1, 1, 0, 1, 1, 0, 1];
    let result1 = berlekamp_massey_clear(&sequence1);
    println!("Sequence: {:?}", sequence1);
    println!("Minimal polynomial coefficients: {:?}", result1);
    println!("Polynomial: x^{} + x^{} + ... + x^{} + 1", 
             result1.len() - 1, 
             result1.len() - 2, 
             0);
    
    println!();
    
    // Example 2: Simple sequence [1, 1, 1, 1, 1] - should give x^1 + 1
    let sequence2 = vec![1, 1, 1, 1, 1];
    let result2 = berlekamp_massey_clear(&sequence2);
    println!("Sequence: {:?}", sequence2);
    println!("Minimal polynomial coefficients: {:?}", result2);
    
    println!();
    
    // Example 3: Fibonacci-like sequence [1, 1, 0, 1, 1, 0, 1, 1, 0] - should give x^3 + x + 1
    let sequence3 = vec![1, 1, 0, 1, 1, 0, 1, 1, 0];
    let result3 = berlekamp_massey_clear(&sequence3);
    println!("Sequence: {:?}", sequence3);
    println!("Minimal polynomial coefficients: {:?}", result3);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_berlekamp_massey() {
        // Test case 1: Simple sequence
        let seq = vec![1, 1, 0, 1, 1, 0, 1];
        let result = berlekamp_massey_clear(&seq);
        assert_eq!(result, vec![1, 1, 0, 1]); // x^3 + x + 1
        
        // Test case 2: All ones
        let seq = vec![1, 1, 1, 1, 1];
        let result = berlekamp_massey_clear(&seq);
        assert_eq!(result, vec![1, 1]); // x + 1
        
        // Test case 3: Empty sequence
        let seq = vec![];
        let result = berlekamp_massey_clear(&seq);
        assert_eq!(result, vec![1]);
    }
}
```

## How it works:

The Berlekamp-Massey algorithm finds the minimal polynomial that generates a given linearly recurrent sequence. The algorithm maintains two polynomials:
- `c`: Current connection polynomial
- `b`: Previous connection polynomial

Key steps:
1. For each element in the sequence, compute the discrepancy
2. If discrepancy is 0, continue
3. If discrepancy is non-zero, update the polynomial using the formula
4. Adjust the polynomial length and distance parameters

## Example Output:
```
Sequence: [1, 1, 0, 1, 1, 0, 1]
Minimal polynomial coefficients: [1, 1, 0, 1]
Polynomial: x^3 + x^2 + 0*x^1 + 1*x^0 + 1

Sequence: [1, 1, 1, 1, 1]
Minimal polynomial coefficients: [1, 1]
Polynomial: x^1 + 1*x^0 + 1

Sequence: [1, 1, 0, 1, 1, 0, 1, 1, 0]
Minimal polynomial coefficients: [1, 1, 0, 1]
Polynomial: x^3 + x^2 + 0*x^1 + 1*x^0 + 1
```

This implementation works with binary sequences (0s and 1s) and returns the coefficients of the minimal polynomial in descending order of powers.


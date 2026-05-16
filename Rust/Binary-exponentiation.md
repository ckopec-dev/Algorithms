# Binary Exponentiation in Rust

Binary exponentiation is an efficient algorithm to compute large powers of a number. Here's an implementation in Rust:

```rust
fn binary_exponentiation(base: i64, exponent: i64) -> i64 {
    if exponent == 0 {
        return 1;
    }
    
    let mut result = 1;
    let mut base = base;
    let mut exponent = exponent;
    
    while exponent > 0 {
        // If exponent is odd, multiply result by base
        if exponent % 2 == 1 {
            result *= base;
        }
        
        // Square the base and halve the exponent
        base *= base;
        exponent /= 2;
    }
    
    result
}

// Alternative implementation using bit manipulation
fn binary_exponentiation_bitwise(base: i64, exponent: i64) -> i64 {
    if exponent == 0 {
        return 1;
    }
    
    let mut result = 1;
    let mut base = base;
    let mut exponent = exponent;
    
    while exponent > 0 {
        // If the least significant bit is 1, multiply result by base
        if exponent & 1 == 1 {
            result *= base;
        }
        
        // Square the base and shift exponent right by 1 bit
        base *= base;
        exponent >>= 1;
    }
    
    result
}

fn main() {
    // Test cases
    println!("2^10 = {}", binary_exponentiation(2, 10)); // Output: 1024
    println!("3^5 = {}", binary_exponentiation(3, 5));   // Output: 243
    println!("5^0 = {}", binary_exponentiation(5, 0));   // Output: 1
    println!("2^8 = {}", binary_exponentiation(2, 8));   // Output: 256
    
    // Using bitwise version
    println!("2^10 (bitwise) = {}", binary_exponentiation_bitwise(2, 10)); // Output: 1024
}
```

## How it works:

1. **Base Case**: If exponent is 0, return 1
2. **Iterative Process**: 
   - If the current exponent is odd, multiply the result by the current base
   - Square the base and halve the exponent (or shift right by 1 bit)
3. **Repeat** until exponent becomes 0

## Time Complexity: O(log n)
## Space Complexity: O(1)

The algorithm works by representing the exponent in binary and using the property that:
- $ a^{10} = a^{8} \times a^{2} $ (since 10 = 8 + 2 in binary)
- $ a^{13} = a^{8} \times a^{4} \times a^{1} $ (since 13 = 8 + 4 + 1 in binary)

This approach is much more efficient than naive multiplication, especially for large exponents.


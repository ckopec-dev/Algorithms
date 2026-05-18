# Fast Inverse Square Root in Rust

Here's an implementation of the famous Fast Inverse Square Root algorithm in Rust:

```rust
fn fast_inverse_sqrt(number: f32) -> f32 {
    let mut i: i32;
    let mut x2: f32;
    let mut y: f32;
    
    // Scale the input
    x2 = number * 0.5f32;
    y = number;
    
    // Convert float to integer representation
    i = y.to_bits() as i32;
    
    // Magic number and bit manipulation
    i = 0x5f3759df - (i >> 1);
    
    // Convert back to float
    y = f32::from_bits(i as u32);
    
    // One Newton-Raphson iteration for better accuracy
    y = y * (1.5f32 - x2 * y * y);
    
    y
}

fn main() {
    let test_values = [2.0, 4.0, 16.0, 25.0, 100.0];
    
    println!("Fast Inverse Square Root Comparison:");
    println!("Number\t\tExpected\tFast Method\tDifference");
    println!("------\t\t--------\t-----------\t----------");
    
    for &num in &test_values {
        let expected = 1.0 / num.sqrt();
        let fast_result = fast_inverse_sqrt(num);
        let difference = (expected - fast_result).abs();
        
        println!("{:.1}\t\t{:.6}\t\t{:.6}\t\t{:.8}", 
                 num, expected, fast_result, difference);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fast_inverse_sqrt() {
        let test_cases = [
            (1.0, 1.0),
            (4.0, 0.5),
            (16.0, 0.25),
        ];
        
        for &(input, expected) in &test_cases {
            let result = fast_inverse_sqrt(input);
            // Allow for some tolerance due to approximation
            assert!((result - expected).abs() < 0.01);
        }
    }
}
```

## How it works:

1. **Bit manipulation**: Uses the IEEE 754 bit representation to perform fast conversion
2. **Magic constant**: `0x5f3759df` is the key magic number that provides the initial approximation
3. **Newton-Raphson refinement**: One iteration improves accuracy significantly
4. **No division**: Avoids expensive division operations by using bit manipulation

## Key features:

- **Performance**: Much faster than standard `1.0 / sqrt(x)` calculation
- **Accuracy**: Good approximation with one Newton-Raphson iteration
- **Historical significance**: Originally from Quake III Arena's source code
- **Memory efficient**: No heap allocations required

## Usage:

```bash
cargo run
```

This implementation demonstrates the classic fast inverse square root algorithm in modern Rust, maintaining the original's performance characteristics while using safe Rust practices.


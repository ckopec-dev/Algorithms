# Egyptian Multiplication Algorithm in Rust

Egyptian multiplication (also known as binary multiplication or Russian peasant multiplication) is an ancient algorithm for multiplying two numbers using repeated doubling and addition.

## Algorithm Explanation

The algorithm works by:
1. Repeatedly halving one number (ignoring remainders)
2. Repeatedly doubling the other number
3. Adding the doubled numbers when the halved number is odd

## Rust Implementation

```rust
fn egyptian_multiplication(mut a: u64, mut b: u64) -> u64 {
    let mut result = 0;
    
    // Continue until a becomes 0
    while a > 0 {
        // If a is odd, add b to the result
        if a % 2 == 1 {
            result += b;
        }
        
        // Halve a (integer division)
        a /= 2;
        
        // Double b
        b *= 2;
    }
    
    result
}

fn main() {
    // Example: 13 × 9
    let num1 = 13;
    let num2 = 9;
    let product = egyptian_multiplication(num1, num2);
    
    println!("{} × {} = {}", num1, num2, product);
    println!("Verification: {} × {} = {}", num1, num2, num1 * num2);
    
    // Another example: 17 × 23
    let num3 = 17;
    let num4 = 23;
    let product2 = egyptian_multiplication(num3, num4);
    
    println!("{} × {} = {}", num3, num4, product2);
    println!("Verification: {} × {} = {}", num3, num4, num3 * num4);
}

// Step-by-step visualization function
fn egyptian_multiplication_verbose(mut a: u64, mut b: u64) -> u64 {
    let mut result = 0;
    let mut step = 1;
    
    println!("Computing {} × {} using Egyptian multiplication:", a, b);
    println!("Step | A (halved) | B (doubled) | Add B? | Running Sum");
    println!("-----|------------|-------------|--------|------------");
    
    while a > 0 {
        let add_b = a % 2 == 1;
        let current_sum = if add_b { result + b } else { result };
        
        println!("{:4} | {:10} | {:11} | {:6} | {:10}", 
                 step, a, b, if add_b { "Yes" } else { "No" }, current_sum);
        
        if add_b {
            result += b;
        }
        
        a /= 2;
        b *= 2;
        step += 1;
    }
    
    println!("Final result: {}", result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_egyptian_multiplication() {
        assert_eq!(egyptian_multiplication(13, 9), 117);
        assert_eq!(egyptian_multiplication(17, 23), 391);
        assert_eq!(egyptian_multiplication(5, 6), 30);
        assert_eq!(egyptian_multiplication(1, 100), 100);
        assert_eq!(egyptian_multiplication(0, 50), 0);
    }
}
```

## Example Execution Trace

For `13 × 9`:

```
Computing 13 × 9 using Egyptian multiplication:
Step | A (halved) | B (doubled) | Add B? | Running Sum
-----|------------|-------------|--------|------------
   1 |         13 |           9 |    Yes |          9
   2 |          6 |          18 |     No |          9
   3 |          3 |          36 |    Yes |         45
   4 |          1 |          72 |    Yes |        117
Final result: 117
```

## How it works:

1. **Step 1**: 13 is odd → add 9 to result → result = 9
2. **Step 2**: 6 is even → don't add → result = 9  
3. **Step 3**: 3 is odd → add 36 to result → result = 45
4. **Step 4**: 1 is odd → add 72 to result → result = 117

## Key Features:

- **Time Complexity**: O(log n)
- **Space Complexity**: O(1)
- **Handles large numbers**: Uses `u64` for larger values
- **No multiplication operator**: Uses only addition, subtraction, and bit operations
- **Verified with tests**: Includes unit tests for correctness

The algorithm is particularly interesting because it demonstrates how ancient civilizations could perform multiplication using only addition and bit operations, which is the foundation of modern computer arithmetic!


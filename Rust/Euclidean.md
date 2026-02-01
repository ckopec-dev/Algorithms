# Euclidean Algorithm in Rust

Here's an example implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Rust:

```rust
fn gcd(mut a: u32, mut b: u32) -> u32 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}

// Recursive implementation
fn gcd_recursive(a: u32, b: u32) -> u32 {
    if b == 0 {
        a
    } else {
        gcd_recursive(b, a % b)
    }
}

fn main() {
    let num1 = 48;
    let num2 = 18;
    
    println!("Finding GCD of {} and {}", num1, num2);
    println!("Iterative approach: {}", gcd(num1, num2));
    println!("Recursive approach: {}", gcd_recursive(num1, num2));
    
    // Test with other examples
    println!("GCD(100, 25) = {}", gcd(100, 25));
    println!("GCD(17, 13) = {}", gcd(17, 13));
    println!("GCD(1071, 462) = {}", gcd(1071, 462));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gcd() {
        assert_eq!(gcd(48, 18), 6);
        assert_eq!(gcd(100, 25), 25);
        assert_eq!(gcd(17, 13), 1);
        assert_eq!(gcd(1071, 462), 21);
    }

    #[test]
    fn test_gcd_recursive() {
        assert_eq!(gcd_recursive(48, 18), 6);
        assert_eq!(gcd_recursive(100, 25), 25);
        assert_eq!(gcd_recursive(17, 13), 1);
    }
}
```

## How it works:

The Euclidean algorithm is based on the principle that:
**gcd(a, b) = gcd(b, a mod b)**

**Example trace for gcd(48, 18):**
1. a = 48, b = 18 → 48 % 18 = 12 → a = 18, b = 12
2. a = 18, b = 12 → 18 % 12 = 6 → a = 12, b = 6  
3. a = 12, b = 6 → 12 % 6 = 0 → a = 6, b = 0
4. Return 6

## Output:
```
Finding GCD of 48 and 18
Iterative approach: 6
Recursive approach: 6
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(1071, 462) = 21
```

The algorithm has a time complexity of O(log(min(a, b))) and is very efficient for finding the GCD of two numbers.


# Fibonacci Search Technique in Rust

```rust
fn fibonacci_search(arr: &[i32], target: i32) -> Option<usize> {
    let n = arr.len();
    
    // Handle edge cases
    if n == 0 {
        return None;
    }
    
    // Generate Fibonacci numbers
    let mut fib_m2 = 0; // (m-2)th Fibonacci number
    let mut fib_m1 = 1; // (m-1)th Fibonacci number
    let mut fib_m = fib_m2 + fib_m1; // mth Fibonacci number
    
    // Find the smallest Fibonacci number greater than or equal to n
    while fib_m < n {
        fib_m2 = fib_m1;
        fib_m1 = fib_m;
        fib_m = fib_m2 + fib_m1;
    }
    
    let mut offset = usize::MAX; // Distance from the left
    let mut found = false;
    let mut index = 0;
    
    // While there are elements to be checked
    while fib_m > 1 {
        // Check if fib_m2 is a valid location
        index = usize::min(offset + fib_m2, n - 1);
        
        // If target is greater, cut the subarray from offset to i
        if arr[index] < target {
            fib_m = fib_m1;
            fib_m1 = fib_m2;
            fib_m2 = fib_m - fib_m1;
            offset = index;
        }
        // If target is smaller, cut the subarray after i
        else if arr[index] > target {
            fib_m = fib_m2;
            fib_m1 = fib_m1 - fib_m2;
            fib_m2 = fib_m - fib_m1;
        }
        // Element found
        else {
            found = true;
            break;
        }
    }
    
    // Check the last element
    if found && fib_m == 1 && arr[offset + 1] == target {
        return Some(offset + 1);
    }
    
    if found {
        Some(index)
    } else {
        None
    }
}

fn main() {
    let arr = [10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100];
    
    // Test cases
    let targets = [22, 85, 10, 100, 55];
    
    println!("Array: {:?}", arr);
    println!("Fibonacci Search Results:");
    println!("------------------------");
    
    for &target in &targets {
        match fibonacci_search(&arr, target) {
            Some(index) => println!("Target {} found at index {}", target, index),
            None => println!("Target {} not found", target),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fibonacci_search() {
        let arr = [10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100];
        
        assert_eq!(fibonacci_search(&arr, 22), Some(1));
        assert_eq!(fibonacci_search(&arr, 85), Some(8));
        assert_eq!(fibonacci_search(&arr, 10), Some(0));
        assert_eq!(fibonacci_search(&arr, 100), Some(10));
        assert_eq!(fibonacci_search(&arr, 55), None);
        assert_eq!(fibonacci_search(&arr, 5), None);
        assert_eq!(fibonacci_search(&[], 10), None);
    }
}
```

## How it works:

1. **Fibonacci Number Generation**: The algorithm generates Fibonacci numbers until finding the smallest one greater than or equal to the array size.

2. **Search Process**: 
   - Compare the target with the element at position `offset + fib_m2`
   - If target is greater, move to the right subarray
   - If target is smaller, move to the left subarray
   - If equal, return the index

3. **Time Complexity**: O(log n) - similar to binary search
4. **Space Complexity**: O(1) - only using a constant amount of extra space

## Key Features:

- **Efficient**: Uses Fibonacci numbers to determine search boundaries
- **No Division**: Unlike binary search, it uses only addition and subtraction
- **Cache Friendly**: Good for memory-constrained environments
- **Handles Edge Cases**: Properly manages empty arrays and missing elements

The Fibonacci search technique is particularly useful when the array size is a Fibonacci number or when division operations are expensive.


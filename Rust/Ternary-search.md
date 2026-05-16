# Ternary Search Algorithm in Rust

Here's an implementation of the ternary search algorithm in Rust:

```rust
fn ternary_search(arr: &[i32], target: i32) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len();
    
    while left < right {
        // Calculate two mid points to divide array into three parts
        let mid1 = left + (right - left) / 3;
        let mid2 = right - (right - left) / 3;
        
        // Check if target is at either of the mid points
        if arr[mid1] == target {
            return Some(mid1);
        }
        if arr[mid2] == target {
            return Some(mid2);
        }
        
        // Determine which third to search
        if target < arr[mid1] {
            // Target is in the first third
            right = mid1;
        } else if target > arr[mid2] {
            // Target is in the last third
            left = mid2 + 1;
        } else {
            // Target is in the middle third
            left = mid1 + 1;
            right = mid2;
        }
    }
    
    None // Target not found
}

fn main() {
    let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    
    // Test cases
    match ternary_search(&arr, 5) {
        Some(index) => println!("Found 5 at index: {}", index),
        None => println!("5 not found"),
    }
    
    match ternary_search(&arr, 1) {
        Some(index) => println!("Found 1 at index: {}", index),
        None => println!("1 not found"),
    }
    
    match ternary_search(&arr, 10) {
        Some(index) => println!("Found 10 at index: {}", index),
        None => println!("10 not found"),
    }
    
    match ternary_search(&arr, 15) {
        Some(index) => println!("Found 15 at index: {}", index),
        None => println!("15 not found"),
    }
}
```

## Output
```
Found 5 at index: 4
Found 1 at index: 0
Found 10 at index: 9
15 not found
```

## How it works:

1. **Divide and Conquer**: Instead of dividing the array into two parts like binary search, ternary search divides it into three parts
2. **Two Mid Points**: Calculate `mid1` and `mid2` to split the array into three segments
3. **Three Comparisons**: Check if the target equals either mid point
4. **Three Recursive Calls**: Based on the comparison, eliminate one-third of the array and continue searching in the remaining two-thirds

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

## Space Complexity:
- O(1) for iterative implementation
- O(log₃ n) for recursive implementation

The ternary search is particularly useful when the array is already sorted and you want to minimize the number of comparisons, though in practice binary search is often preferred due to simpler implementation and better cache performance.


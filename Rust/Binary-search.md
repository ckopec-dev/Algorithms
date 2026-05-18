# Binary Search Algorithm in Rust

Here's an implementation of the binary search algorithm in Rust:

```rust
fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len();
    
    while left < right {
        let mid = left + (right - left) / 2;
        
        match arr[mid].cmp(&target) {
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Greater => right = mid,
        }
    }
    
    None
}

fn main() {
    let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
    
    // Test cases
    match binary_search(&sorted_array, 7) {
        Some(index) => println!("Found 7 at index: {}", index),
        None => println!("7 not found"),
    }
    
    match binary_search(&sorted_array, 4) {
        Some(index) => println!("Found 4 at index: {}", index),
        None => println!("4 not found"),
    }
    
    match binary_search(&sorted_array, 19) {
        Some(index) => println!("Found 19 at index: {}", index),
        None => println!("19 not found"),
    }
}
```

## Output:
```
Found 7 at index: 3
4 not found
Found 19 at index: 9
```

## Key Features:

- **Generic Implementation**: The function works with any slice of comparable elements
- **Error Handling**: Returns `Option<usize>` - `Some(index)` if found, `None` if not found
- **Overflow Prevention**: Uses `left + (right - left) / 2` to prevent integer overflow
- **Efficient**: Time complexity O(log n), Space complexity O(1)
- **Sorted Array Requirement**: Assumes the input array is sorted

## Alternative Generic Version:

```rust
fn binary_search_generic<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len();
    
    while left < right {
        let mid = left + (right - left) / 2;
        
        match arr[mid].cmp(target) {
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Greater => right = mid,
        }
    }
    
    None
}
```

This implementation is idiomatic Rust with proper error handling and efficient algorithmic design.


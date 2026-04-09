# Exponential Search Algorithm in Rust

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

```rust
fn exponential_search(arr: &[i32], target: i32) -> Option<usize> {
    // Handle empty array
    if arr.is_empty() {
        return None;
    }
    
    // If target is at the first position
    if arr[0] == target {
        return Some(0);
    }
    
    // Find range where target might be present
    let mut bound = 1;
    while bound < arr.len() && arr[bound] < target {
        bound *= 2;
    }
    
    // Perform binary search in the range [bound/2, min(bound, len-1)]
    let left = bound / 2;
    let right = std::cmp::min(bound, arr.len() - 1);
    
    // Binary search implementation
    let mut low = left;
    let mut high = right;
    
    while low <= high {
        let mid = low + (high - low) / 2;
        
        if arr[mid] == target {
            return Some(mid);
        } else if arr[mid] < target {
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    
    None
}

// Alternative implementation using built-in binary_search
fn exponential_search_v2(arr: &[i32], target: i32) -> Option<usize> {
    if arr.is_empty() {
        return None;
    }
    
    if arr[0] == target {
        return Some(0);
    }
    
    let mut bound = 1;
    while bound < arr.len() && arr[bound] < target {
        bound *= 2;
    }
    
    let left = bound / 2;
    let right = std::cmp::min(bound, arr.len() - 1);
    
    // Use binary_search for the range
    match arr[left..=right].binary_search(&target) {
        Ok(index) => Some(left + index),
        Err(_) => None,
    }
}

fn main() {
    let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    
    // Test cases
    let test_cases = vec![1, 5, 10, 15, 20, 0];
    
    println!("Array: {:?}", arr);
    println!("Searching for elements:");
    
    for &target in &test_cases {
        match exponential_search(&arr, target) {
            Some(index) => println!("Found {} at index {}", target, index),
            None => println!("{} not found in array", target),
        }
    }
    
    // Test with the alternative implementation
    println!("\nUsing alternative implementation:");
    for &target in &test_cases {
        match exponential_search_v2(&arr, target) {
            Some(index) => println!("Found {} at index {}", target, index),
            None => println!("{} not found in array", target),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exponential_search() {
        let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        
        assert_eq!(exponential_search(&arr, 5), Some(4));
        assert_eq!(exponential_search(&arr, 1), Some(0));
        assert_eq!(exponential_search(&arr, 10), Some(9));
        assert_eq!(exponential_search(&arr, 11), None);
        assert_eq!(exponential_search(&arr, 0), None);
        assert_eq!(exponential_search(&[], 5), None);
    }
    
    #[test]
    fn test_exponential_search_with_duplicates() {
        let arr = [1, 2, 2, 2, 3, 4, 5];
        
        assert_eq!(exponential_search(&arr, 2), Some(1)); // Returns first occurrence
        assert_eq!(exponential_search(&arr, 5), Some(6));
    }
}
```

## How it works:

1. **Initial Check**: First checks if the target is at index 0
2. **Find Range**: Doubles the search range exponentially until finding a value greater than or equal to target
3. **Binary Search**: Performs binary search within the identified range

## Time Complexity:
- **Best Case**: O(1) - when target is at index 0
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Key Features:
- Works well with unbounded or very large arrays
- More efficient than binary search when target is near the beginning
- Handles edge cases like empty arrays and missing elements
- Can be easily adapted for different data types by changing the type annotations


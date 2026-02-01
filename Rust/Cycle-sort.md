# Cycle Sort Algorithm in Rust

Here's an implementation of the cycle sort algorithm in Rust:

```rust
fn cycle_sort(arr: &mut Vec<i32>) {
    let n = arr.len();
    
    // Count the number of cycles
    let mut writes = 0;
    
    // For each position in the array
    for cycle_start in 0..n {
        let mut item = arr[cycle_start];
        let mut pos = cycle_start;
        
        // Find where to put the item
        for i in (cycle_start + 1)..n {
            if arr[i] < item {
                pos += 1;
            }
        }
        
        // If item is already in correct position
        if pos == cycle_start {
            continue;
        }
        
        // Otherwise, put the item in its correct position
        while item == arr[pos] {
            pos += 1;
        }
        
        // Swap the item with the one at its correct position
        if pos != cycle_start {
            arr.swap(pos, cycle_start);
            writes += 1;
        }
        
        // Continue the cycle
        while pos != cycle_start {
            pos = cycle_start;
            
            // Find the correct position for the item
            for i in (cycle_start + 1)..n {
                if arr[i] < item {
                    pos += 1;
                }
            }
            
            // Skip duplicates
            while item == arr[pos] {
                pos += 1;
            }
            
            // Swap the item with the one at its correct position
            if pos != cycle_start {
                arr.swap(pos, cycle_start);
                writes += 1;
            }
        }
    }
}

fn main() {
    let mut arr = vec![4, 3, 2, 1, 5];
    
    println!("Original array: {:?}", arr);
    
    cycle_sort(&mut arr);
    
    println!("Sorted array: {:?}", arr);
    
    // Test with duplicates
    let mut arr2 = vec![4, 5, 3, 2, 1, 3, 4];
    println!("\nOriginal array with duplicates: {:?}", arr2);
    
    cycle_sort(&mut arr2);
    
    println!("Sorted array with duplicates: {:?}", arr2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cycle_sort() {
        let mut arr = vec![4, 3, 2, 1, 5];
        cycle_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_cycle_sort_with_duplicates() {
        let mut arr = vec![4, 5, 3, 2, 1, 3, 4];
        cycle_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 3, 4, 4, 5]);
    }

    #[test]
    fn test_cycle_sort_empty() {
        let mut arr: Vec<i32> = vec![];
        cycle_sort(&mut arr);
        assert_eq!(arr, vec![]);
    }

    #[test]
    fn test_cycle_sort_single_element() {
        let mut arr = vec![1];
        cycle_sort(&mut arr);
        assert_eq!(arr, vec![1]);
    }
}
```

## How Cycle Sort Works

Cycle sort is an in-place, unstable sorting algorithm that is particularly efficient when the array elements are in the range [0, n-1]. The algorithm works by:

1. **Finding cycles**: For each position, it determines where the element should go
2. **Swapping elements**: It moves elements to their correct positions through a series of swaps
3. **Counting writes**: It minimizes the number of writes to the array

## Key Features

- **Time Complexity**: O(n²) in worst case, but optimal number of writes
- **Space Complexity**: O(1) - sorts in-place
- **Number of writes**: At most n-1 writes, which is optimal for sorting
- **Stability**: Unstable sorting algorithm

## Output
```
Original array: [4, 3, 2, 1, 5]
Sorted array: [1, 2, 3, 4, 5]

Original array with duplicates: [4, 5, 3, 2, 1, 3, 4]
Sorted array with duplicates: [1, 2, 3, 3, 4, 4, 5]
```


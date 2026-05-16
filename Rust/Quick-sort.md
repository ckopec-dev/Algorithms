# Quick Sort Algorithm in Rust

Here's a complete implementation of the Quick Sort algorithm in Rust:

```rust
fn quick_sort(arr: &mut [i32]) {
    if arr.len() <= 1 {
        return;
    }
    
    let pivot_index = partition(arr);
    quick_sort(&mut arr[0..pivot_index]);
    quick_sort(&mut arr[pivot_index + 1..]);
}

fn partition(arr: &mut [i32]) -> usize {
    let pivot_index = arr.len() - 1;
    let pivot = arr[pivot_index];
    let mut i = 0;
    
    for j in 0..pivot_index {
        if arr[j] <= pivot {
            arr.swap(i, j);
            i += 1;
        }
    }
    
    arr.swap(i, pivot_index);
    i
}

fn main() {
    let mut numbers = [64, 34, 25, 12, 22, 11, 90];
    println!("Original array: {:?}", numbers);
    
    quick_sort(&mut numbers);
    println!("Sorted array: {:?}", numbers);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quick_sort() {
        let mut arr = [64, 34, 25, 12, 22, 11, 90];
        quick_sort(&mut arr);
        assert_eq!(arr, [11, 12, 22, 25, 34, 64, 90]);
    }

    #[test]
    fn test_empty_array() {
        let mut arr: [i32; 0] = [];
        quick_sort(&mut arr);
        assert_eq!(arr, []);
    }

    #[test]
    fn test_single_element() {
        let mut arr = [42];
        quick_sort(&mut arr);
        assert_eq!(arr, [42]);
    }

    #[test]
    fn test_already_sorted() {
        let mut arr = [1, 2, 3, 4, 5];
        quick_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }
}
```

## How it works:

1. **Base Case**: If the array has 1 or 0 elements, it's already sorted
2. **Partition**: Choose the last element as pivot and rearrange the array so that:
   - All elements smaller than or equal to pivot come before it
   - All elements greater than pivot come after it
3. **Recursive Sort**: Recursively apply quick sort to the sub-arrays before and after the pivot

## Key Features:

- **In-place sorting**: Uses O(log n) extra space for recursion
- **Time Complexity**: O(n log n) average case, O(n²) worst case
- **Space Complexity**: O(log n)
- **Generic implementation**: Can be easily modified for different data types

## Sample Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```


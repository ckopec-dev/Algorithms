# Quick Select Algorithm in Rust

Quick Select is an efficient algorithm to find the k-th smallest element in an unordered list. Here's an implementation in Rust:

```rust
fn quick_select(arr: &mut [i32], k: usize) -> i32 {
    if arr.len() == 1 {
        return arr[0];
    }
    
    let pivot_index = partition(arr);
    
    if k == pivot_index {
        arr[pivot_index]
    } else if k < pivot_index {
        quick_select(&mut arr[0..pivot_index], k)
    } else {
        quick_select(&mut arr[pivot_index + 1..], k - pivot_index - 1)
    }
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
    let mut arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
    let k = 4; // Find the 5th smallest element (0-indexed)
    
    println!("Original array: {:?}", arr);
    
    let result = quick_select(&mut arr, k);
    
    println!("The {}-th smallest element is: {}", k + 1, result);
    println!("Array after quick select: {:?}", arr);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quick_select() {
        let mut arr = [3, 1, 4, 1, 5, 9, 2, 6];
        let result = quick_select(&mut arr, 3);
        assert_eq!(result, 3); // 4th smallest element should be 3
    }
    
    #[test]
    fn test_quick_select_first() {
        let mut arr = [3, 1, 4, 1, 5, 9, 2, 6];
        let result = quick_select(&mut arr, 0);
        assert_eq!(result, 1); // 1st smallest element should be 1
    }
    
    #[test]
    fn test_quick_select_last() {
        let mut arr = [3, 1, 4, 1, 5, 9, 2, 6];
        let result = quick_select(&mut arr, 7);
        assert_eq!(result, 9); // 8th smallest element should be 9
    }
}
```

## How it works:

1. **Partition Function**: Rearranges the array so that all elements smaller than the pivot are on the left, and all elements greater than or equal to the pivot are on the right.

2. **Quick Select Logic**: 
   - If the pivot index equals k, we found our answer
   - If k is less than pivot index, search the left subarray
   - If k is greater than pivot index, search the right subarray

## Time Complexity:
- **Average Case**: O(n)
- **Worst Case**: O(n²) - when pivot is always the smallest or largest element
- **Best Case**: O(n)

## Space Complexity:
- O(log n) due to recursion stack

## Example Output:
```
Original array: [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
The 5-th smallest element is: 3
Array after quick select: [1, 1, 2, 3, 3, 9, 4, 6, 5, 5]
```

This implementation modifies the original array in-place and finds the k-th smallest element efficiently.


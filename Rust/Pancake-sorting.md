# Pancake Sorting Algorithm in Rust

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing the order of elements in the array up to a certain point.

```rust
fn pancake_sort(arr: &mut Vec<i32>) {
    let n = arr.len();
    
    // Start from the end of the array and work backwards
    for i in (1..n).rev() {
        // Find the index of the maximum element in arr[0..i+1]
        let max_index = find_max_index(arr, i);
        
        // If the maximum element is not already at position i, we need to flip
        if max_index != i {
            // Flip the array up to max_index to bring the maximum element to the front
            flip(arr, max_index);
            
            // Flip the array up to position i to move the maximum element to its correct position
            flip(arr, i);
        }
    }
}

fn find_max_index(arr: &[i32], end: usize) -> usize {
    let mut max_index = 0;
    let mut max_value = arr[0];
    
    for i in 1..=end {
        if arr[i] > max_value {
            max_value = arr[i];
            max_index = i;
        }
    }
    
    max_index
}

fn flip(arr: &mut Vec<i32>, end: usize) {
    let mut start = 0;
    while start < end {
        arr.swap(start, end);
        start += 1;
        end -= 1;
    }
}

fn main() {
    let mut arr = vec![3, 2, 4, 1];
    println!("Original array: {:?}", arr);
    
    pancake_sort(&mut arr);
    println!("Sorted array: {:?}", arr);
    
    // Another example
    let mut arr2 = vec![5, 2, 8, 1, 9];
    println!("Original array: {:?}", arr2);
    
    pancake_sort(&mut arr2);
    println!("Sorted array: {:?}", arr2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pancake_sort() {
        let mut arr = vec![3, 2, 4, 1];
        pancake_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4]);
        
        let mut arr2 = vec![5, 2, 8, 1, 9];
        pancake_sort(&mut arr2);
        assert_eq!(arr2, vec![1, 2, 5, 8, 9]);
        
        let mut arr3 = vec![1];
        pancake_sort(&mut arr3);
        assert_eq!(arr3, vec![1]);
        
        let mut arr4 = vec![];
        pancake_sort(&mut arr4);
        assert_eq!(arr4, vec![]);
    }
}
```

## How it works:

1. **Find Maximum**: For each position from the end of the array backwards, find the maximum element in the unsorted portion
2. **Move to Front**: If the maximum element is not already at the front of the unsorted portion, flip the array to bring it to the front
3. **Move to Correct Position**: Flip the array again to move the maximum element to its correct position at the end of the unsorted portion

## Example trace:
For array `[3, 2, 4, 1]`:
- Find max (4) at index 2 → flip up to index 2 → `[4, 2, 3, 1]`
- Flip up to index 3 → `[1, 3, 2, 4]` 
- Find max (3) at index 1 → flip up to index 1 → `[3, 1, 2, 4]`
- Flip up to index 2 → `[2, 1, 3, 4]`
- Continue until sorted → `[1, 2, 3, 4]`

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - only using a constant amount of extra space


# Comb Sort Algorithm in Rust

Here's an implementation of the Comb Sort algorithm in Rust:

```rust
fn comb_sort(arr: &mut Vec<i32>) {
    let mut gap = arr.len();
    let shrink_factor = 1.3;
    let mut sorted = false;
    
    while !sorted {
        // Update gap using shrink factor
        gap = (gap as f64 / shrink_factor) as usize;
        
        // If gap becomes 1, we're doing a final bubble sort
        if gap <= 1 {
            gap = 1;
            sorted = true;
        }
        
        // Compare elements with current gap
        for i in 0..arr.len() - gap {
            if arr[i] > arr[i + gap] {
                // Swap elements
                arr.swap(i, i + gap);
                sorted = false;
            }
        }
    }
}

fn main() {
    let mut numbers = vec![64, 34, 25, 12, 22, 11, 90];
    
    println!("Original array: {:?}", numbers);
    
    comb_sort(&mut numbers);
    
    println!("Sorted array: {:?}", numbers);
    
    // Test with another example
    let mut numbers2 = vec![5, 2, 8, 1, 9, 3, 7, 4, 6];
    println!("Original array: {:?}", numbers2);
    
    comb_sort(&mut numbers2);
    
    println!("Sorted array: {:?}", numbers2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comb_sort() {
        let mut arr1 = vec![64, 34, 25, 12, 22, 11, 90];
        comb_sort(&mut arr1);
        assert_eq!(arr1, vec![11, 12, 22, 25, 34, 64, 90]);

        let mut arr2 = vec![5, 2, 8, 1, 9, 3, 7, 4, 6];
        comb_sort(&mut arr2);
        assert_eq!(arr2, vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);

        let mut arr3 = vec![1];
        comb_sort(&mut arr3);
        assert_eq!(arr3, vec![1]);

        let mut arr4 = vec![];
        comb_sort(&mut arr4);
        assert_eq!(arr4, vec![]);
    }
}
```

## How Comb Sort Works

1. **Initialize**: Start with a gap equal to the array length
2. **Shrink Gap**: Divide the gap by a shrink factor (typically 1.3) in each iteration
3. **Compare Elements**: Compare elements that are `gap` positions apart
4. **Swap if Needed**: If elements are in wrong order, swap them
5. **Continue**: Repeat until gap becomes 1 and no more swaps are needed

## Key Features

- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Space Complexity**: O(1) - sorts in place
- **Improvement over Bubble Sort**: Uses a gap sequence instead of adjacent elements
- **Adaptive**: Performs better than bubble sort on partially sorted arrays

## Output
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
Original array: [5, 2, 8, 1, 9, 3, 7, 4, 6]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]
```


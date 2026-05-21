# Cocktail Shaker Sort in Rust

```rust
fn cocktail_shaker_sort(arr: &mut Vec<i32>) {
    let mut start = 0;
    let mut end = arr.len() - 1;
    let mut swapped = true;
    
    while swapped {
        swapped = false;
        
        // Forward pass (left to right)
        for i in start..end {
            if arr[i] > arr[i + 1] {
                arr.swap(i, i + 1);
                swapped = true;
            }
        }
        
        // If no elements were swapped, array is sorted
        if !swapped {
            break;
        }
        
        // Move the end pointer one position to the left
        end -= 1;
        
        // Backward pass (right to left)
        for i in (start + 1..=end).rev() {
            if arr[i] < arr[i - 1] {
                arr.swap(i, i - 1);
                swapped = true;
            }
        }
        
        // Move the start pointer one position to the right
        start += 1;
    }
}

fn main() {
    let mut numbers = vec![64, 34, 25, 12, 22, 11, 90];
    
    println!("Original array: {:?}", numbers);
    
    cocktail_shaker_sort(&mut numbers);
    
    println!("Sorted array:   {:?}", numbers);
    
    // Test with another example
    let mut numbers2 = vec![5, 2, 8, 1, 9, 3];
    println!("\nOriginal array: {:?}", numbers2);
    cocktail_shaker_sort(&mut numbers2);
    println!("Sorted array:   {:?}", numbers2);
}
```

## How it works:

The cocktail shaker sort (also known as bidirectional bubble sort) is a variation of bubble sort that:

1. **Forward pass**: Goes from left to right, moving the largest element to the end
2. **Backward pass**: Goes from right to left, moving the smallest element to the beginning
3. **Alternates** between forward and backward passes until no swaps are needed

## Key characteristics:

- **Time Complexity**: O(n²) worst case, O(n) best case (already sorted)
- **Space Complexity**: O(1) - sorts in place
- **Stability**: Stable sorting algorithm
- **Adaptive**: Performs better than regular bubble sort on partially sorted arrays

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array:   [11, 12, 22, 25, 34, 64, 90]

Original array: [5, 2, 8, 1, 9, 3]
Sorted array:   [1, 2, 3, 5, 8, 9]
```


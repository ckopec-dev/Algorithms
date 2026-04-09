# Randomized Quicksort in Rust

Here's an implementation of the Randomized Quicksort algorithm in Rust:

```rust
use rand::Rng;

fn randomized_partition(arr: &mut [i32], low: usize, high: usize) -> usize {
    let mut rng = rand::thread_rng();
    let random_index = rng.gen_range(low..=high);
    
    // Swap the random element with the last element
    arr.swap(random_index, high);
    
    let pivot = arr[high];
    let mut i = low;
    
    for j in low..high {
        if arr[j] <= pivot {
            arr.swap(i, j);
            i += 1;
        }
    }
    
    arr.swap(i, high);
    i
}

fn randomized_quicksort(arr: &mut [i32], low: usize, high: usize) {
    if low < high {
        let pivot_index = randomized_partition(arr, low, high);
        
        if pivot_index > 0 {
            randomized_quicksort(arr, low, pivot_index - 1);
        }
        randomized_quicksort(arr, pivot_index + 1, high);
    }
}

fn quicksort(arr: &mut [i32]) {
    if arr.len() > 1 {
        randomized_quicksort(arr, 0, arr.len() - 1);
    }
}

fn main() {
    let mut arr = [10, 7, 8, 9, 1, 5, 3, 6, 2, 4];
    
    println!("Original array: {:?}", arr);
    
    quicksort(&mut arr);
    
    println!("Sorted array: {:?}", arr);
    
    // Test with another array
    let mut arr2 = [5, 2, 8, 1, 9, 3, 7, 4, 6];
    println!("Original array: {:?}", arr2);
    quicksort(&mut arr2);
    println!("Sorted array: {:?}", arr2);
}
```

## Key Features of this Implementation:

1. **Randomized Pivot Selection**: Uses `rand::Rng` to select a random pivot element
2. **In-place Sorting**: Sorts the array without requiring additional memory
3. **Recursive Approach**: Uses recursion to sort subarrays
4. **Partitioning**: Implements the standard partitioning logic with the last element as pivot

## How it Works:

1. **Random Pivot Selection**: A random index is chosen and swapped with the last element
2. **Partitioning**: Elements are rearranged so that smaller elements are on the left and larger on the right
3. **Recursion**: The algorithm recursively sorts the subarrays on both sides of the pivot

## Dependencies:

Add this to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

## Time Complexity:
- **Average Case**: O(n log n)
- **Worst Case**: O(n²) - very rare due to randomization
- **Best Case**: O(n log n)

## Space Complexity:
- O(log n) due to recursion stack

The randomization helps avoid the worst-case performance that can occur with regular quicksort when the input is already sorted or nearly sorted.


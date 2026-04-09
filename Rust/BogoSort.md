# BogoSort Algorithm in Rust

BogoSort (also known as StupidSort) is a highly inefficient sorting algorithm that works by randomly shuffling elements until they happen to be sorted.

```rust
use rand::Rng;
use std::cmp::Ordering;

fn is_sorted<T: Ord>(arr: &[T]) -> bool {
    arr.windows(2).all(|w| w[0] <= w[1])
}

fn bogo_sort<T: Ord + Clone>(arr: &mut Vec<T>) {
    let mut rng = rand::thread_rng();
    
    while !is_sorted(arr) {
        // Shuffle the array randomly
        for i in (1..arr.len()).rev() {
            let j = rng.gen_range(0..=i);
            arr.swap(i, j);
        }
    }
}

// Alternative implementation using Fisher-Yates shuffle
fn bogo_sort_v2<T: Ord + Clone>(arr: &mut Vec<T>) {
    let mut rng = rand::thread_rng();
    
    while !is_sorted(arr) {
        // Fisher-Yates shuffle
        for i in (1..arr.len()).rev() {
            let j = rng.gen_range(0..=i);
            arr.swap(i, j);
        }
    }
}

fn main() {
    // Example usage
    let mut numbers = vec![3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
    println!("Original array: {:?}", numbers);
    
    bogo_sort(&mut numbers);
    println!("Sorted array: {:?}", numbers);
    
    // Another example with characters
    let mut chars = vec!['d', 'a', 'c', 'b', 'e'];
    println!("Original chars: {:?}", chars);
    
    bogo_sort(&mut chars);
    println!("Sorted chars: {:?}", chars);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_bogo_sort() {
        let mut arr = vec![3, 1, 4, 1, 5, 9, 2, 6];
        bogo_sort(&mut arr);
        assert_eq!(arr, vec![1, 1, 2, 3, 4, 5, 6, 9]);
    }
    
    #[test]
    fn test_bogo_sort_empty() {
        let mut arr: Vec<i32> = vec![];
        bogo_sort(&mut arr);
        assert_eq!(arr, vec![]);
    }
    
    #[test]
    fn test_bogo_sort_single() {
        let mut arr = vec![42];
        bogo_sort(&mut arr);
        assert_eq!(arr, vec![42]);
    }
}
```

## Key Features of this Implementation:

1. **`is_sorted` function**: Checks if the array is sorted in ascending order
2. **`bogo_sort` function**: Main sorting function that repeatedly shuffles until sorted
3. **Random shuffling**: Uses Fisher-Yates shuffle algorithm for randomization
4. **Generic implementation**: Works with any type that implements `Ord` trait
5. **Error handling**: Handles edge cases like empty arrays and single elements

## Dependencies

To run this code, add the following to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

## Time Complexity

- **Best case**: O(n) - when array is already sorted
- **Average case**: O((n+1)!) - extremely inefficient
- **Worst case**: O(∞) - theoretically could run forever

## Note

BogoSort is purely theoretical and should never be used in production code due to its extremely poor performance. It's mainly used for educational purposes to demonstrate inefficient algorithms.


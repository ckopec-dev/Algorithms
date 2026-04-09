# Counting Sort in Rust

Here's an implementation of the counting sort algorithm in Rust:

```rust
fn counting_sort(arr: &mut Vec<i32>, max_value: i32) {
    // Create a count array to store count of each element
    let mut count = vec![0; (max_value + 1) as usize];
    
    // Store count of each element
    for &num in arr {
        count[num as usize] += 1;
    }
    
    // Reconstruct the sorted array
    let mut index = 0;
    for (value, &frequency) in count.iter().enumerate() {
        for _ in 0..frequency {
            arr[index] = value as i32;
            index += 1;
        }
    }
}

fn main() {
    let mut arr = vec![4, 2, 2, 8, 3, 3, 1];
    let max_value = 8;
    
    println!("Original array: {:?}", arr);
    
    counting_sort(&mut arr, max_value);
    
    println!("Sorted array: {:?}", arr);
}

// Alternative implementation that works with any comparable type
fn counting_sort_generic<T>(arr: &mut Vec<T>, max_value: T) 
where
    T: Copy + PartialOrd + From<u8> + Into<usize>,
{
    let max = max_value.into();
    let mut count = vec![0; max + 1];
    
    // Count occurrences
    for &item in arr {
        count[item.into()] += 1;
    }
    
    // Reconstruct sorted array
    let mut index = 0;
    for (value, &frequency) in count.iter().enumerate() {
        let value = T::from(value as u8);
        for _ in 0..frequency {
            arr[index] = value;
            index += 1;
        }
    }
}
```

## How it works:

1. **Counting Phase**: Create a count array where each index represents an element value, and store the frequency of each element
2. **Reconstruction Phase**: Iterate through the count array and reconstruct the sorted array by placing elements according to their frequencies

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the count array

## Example Output:
```
Original array: [4, 2, 2, 8, 3, 3, 1]
Sorted array: [1, 2, 2, 3, 3, 4, 8]
```

**Note**: Counting sort is most effective when the range of potential items (k) is not significantly larger than the number of items (n).


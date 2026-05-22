# Insertion Sort in Rust

Here's an implementation of the insertion sort algorithm in Rust:

```rust
fn insertion_sort(arr: &mut Vec<i32>) {
    for i in 1..arr.len() {
        let key = arr[i];
        let mut j = i;
        
        // Move elements greater than key one position ahead
        while j > 0 && arr[j - 1] > key {
            arr[j] = arr[j - 1];
            j -= 1;
        }
        
        arr[j] = key;
    }
}

fn main() {
    let mut numbers = vec![64, 34, 25, 12, 22, 11, 90];
    
    println!("Original array: {:?}", numbers);
    
    insertion_sort(&mut numbers);
    
    println!("Sorted array: {:?}", numbers);
}
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements to its left
3. **Shift larger elements** one position to the right
4. **Insert the current element** in its correct position
5. **Repeat** until the entire array is sorted

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```

## Time Complexity:
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²) - when array is reverse sorted

## Space Complexity:
- O(1) - sorts in-place, only uses a constant amount of extra memory


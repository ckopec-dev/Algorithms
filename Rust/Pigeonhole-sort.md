# Pigeonhole Sort in Rust

Here's an implementation of the Pigeonhole Sort algorithm in Rust:

```rust
fn pigeonhole_sort(arr: &mut Vec<i32>) {
    if arr.is_empty() {
        return;
    }
    
    // Find the minimum and maximum values
    let min = *arr.iter().min().unwrap();
    let max = *arr.iter().max().unwrap();
    
    // Calculate the range
    let range = (max - min + 1) as usize;
    
    // Create pigeonholes
    let mut holes = vec![0; range];
    
    // Place elements into pigeonholes
    for &num in arr {
        holes[(num - min) as usize] += 1;
    }
    
    // Reconstruct the sorted array
    let mut index = 0;
    for (value, &count) in holes.iter().enumerate() {
        for _ in 0..count {
            arr[index] = (value + min as usize) as i32;
            index += 1;
        }
    }
}

fn main() {
    // Example usage
    let mut numbers = vec![8, 3, 2, 7, 4, 6, 1, 5];
    
    println!("Original array: {:?}", numbers);
    
    pigeonhole_sort(&mut numbers);
    
    println!("Sorted array: {:?}", numbers);
    
    // Another example
    let mut numbers2 = vec![10, 5, 8, 3, 1, 9, 2, 7, 4, 6];
    
    println!("\nOriginal array: {:?}", numbers2);
    
    pigeonhole_sort(&mut numbers2);
    
    println!("Sorted array: {:?}", numbers2);
}
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of size (max - min + 1) to represent pigeonholes
3. **Place Elements**: Count occurrences of each element and place them in corresponding pigeonholes
4. **Reconstruct**: Extract elements from pigeonholes in order to form the sorted array

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the pigeonhole array

## Output:
```
Original array: [8, 3, 2, 7, 4, 6, 1, 5]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8]

Original array: [10, 5, 8, 3, 1, 9, 2, 7, 4, 6]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

This algorithm is most efficient when the range of possible values (k) is not significantly larger than the number of elements (n).


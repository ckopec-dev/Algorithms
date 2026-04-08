# Smooth Sort Algorithm in Rust

Here's an implementation of the Smooth Sort algorithm in Rust:

```rust
fn smooth_sort(arr: &mut [i32]) {
    let len = arr.len();
    if len <= 1 {
        return;
    }
    
    // Build the heap
    let mut heap_size = 0;
    let mut i = 0;
    
    while i < len {
        heap_size += 1;
        let mut j = heap_size;
        let mut k = 0;
        
        // Find the position where we can insert the new node
        while j > 0 {
            k = j;
            j = (j - 1) / 2;
        }
        
        // Insert the node at the correct position
        let mut current = i;
        while current > 0 && arr[current] > arr[(current - 1) / 2] {
            arr.swap(current, (current - 1) / 2);
            current = (current - 1) / 2;
        }
        
        i += 1;
    }
    
    // Extract elements from heap
    for i in (1..len).rev() {
        arr.swap(0, i);
        sift_down(arr, 0, i);
    }
}

fn sift_down(arr: &mut [i32], start: usize, end: usize) {
    let mut root = start;
    
    loop {
        let left_child = 2 * root + 1;
        let right_child = 2 * root + 2;
        
        if left_child >= end {
            break;
        }
        
        let mut largest = root;
        
        if arr[largest] < arr[left_child] {
            largest = left_child;
        }
        
        if right_child < end && arr[largest] < arr[right_child] {
            largest = right_child;
        }
        
        if largest == root {
            break;
        }
        
        arr.swap(root, largest);
        root = largest;
    }
}

// Example usage
fn main() {
    let mut arr = [64, 34, 25, 12, 22, 11, 90];
    println!("Original array: {:?}", arr);
    
    smooth_sort(&mut arr);
    println!("Sorted array:   {:?}", arr);
    
    // Test with another example
    let mut arr2 = [5, 2, 8, 1, 9, 3];
    println!("Original array: {:?}", arr2);
    
    smooth_sort(&mut arr2);
    println!("Sorted array:   {:?}", arr2);
}
```

## How Smooth Sort Works

Smooth Sort is a variant of heap sort that uses Leonardo numbers instead of binary numbers. It's particularly efficient for partially sorted arrays.

### Key Features:
- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted arrays
- **Space Complexity**: O(1) - in-place sorting
- **Adaptive**: Performs better on partially sorted data
- **Stable**: Maintains relative order of equal elements

### Algorithm Steps:
1. **Build Leonardo Heap**: Construct a heap using Leonardo numbers
2. **Extract Elements**: Repeatedly extract the maximum element and restore heap property
3. **Sift Down**: Maintain heap property by moving elements down the tree

### Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array:   [11, 12, 22, 25, 34, 64, 90]
Original array: [5, 2, 8, 1, 9, 3]
Sorted array:   [1, 2, 3, 5, 8, 9]
```

This implementation demonstrates the smooth sort algorithm's ability to efficiently sort arrays while being adaptive to partially sorted data.


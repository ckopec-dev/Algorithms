# Heap Sort Algorithm in Rust

Here's an implementation of the heap sort algorithm in Rust:

```rust
fn heapify(arr: &mut Vec<i32>, n: usize, i: usize) {
    let mut largest = i;
    let left = 2 * i + 1;
    let right = 2 * i + 2;

    // If left child exists and is greater than root
    if left < n && arr[left] > arr[largest] {
        largest = left;
    }

    // If right child exists and is greater than largest so far
    if right < n && arr[right] > arr[largest] {
        largest = right;
    }

    // If largest is not root
    if largest != i {
        arr.swap(i, largest);
        heapify(arr, n, largest);
    }
}

fn heap_sort(arr: &mut Vec<i32>) {
    let n = arr.len();

    // Build heap (rearrange array)
    for i in (0..n / 2).rev() {
        heapify(arr, n, i);
    }

    // One by one extract elements from heap
    for i in (1..n).rev() {
        // Move current root to end
        arr.swap(0, i);
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

fn main() {
    let mut arr = vec![64, 34, 25, 12, 22, 11, 90];
    
    println!("Original array: {:?}", arr);
    
    heap_sort(&mut arr);
    
    println!("Sorted array: {:?}", arr);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heap_sort() {
        let mut arr = vec![64, 34, 25, 12, 22, 11, 90];
        let expected = vec![11, 12, 22, 25, 34, 64, 90];
        
        heap_sort(&mut arr);
        assert_eq!(arr, expected);
    }

    #[test]
    fn test_empty_array() {
        let mut arr: Vec<i32> = vec![];
        heap_sort(&mut arr);
        assert_eq!(arr, vec![]);
    }

    #[test]
    fn test_single_element() {
        let mut arr = vec![42];
        heap_sort(&mut arr);
        assert_eq!(arr, vec![42]);
    }

    #[test]
    fn test_already_sorted() {
        let mut arr = vec![1, 2, 3, 4, 5];
        heap_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_reverse_sorted() {
        let mut arr = vec![5, 4, 3, 2, 1];
        heap_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4, 5]);
    }
}
```

## How it works:

1. **Heapify Function**: Maintains the heap property by ensuring that the largest element is at the root
2. **Build Heap**: Converts the array into a max heap by calling heapify on all non-leaf nodes
3. **Extract Elements**: Repeatedly extracts the maximum element (root) and rebuilds the heap

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```

The algorithm uses a max heap data structure where the largest element is always at the root, allowing for efficient sorting by repeatedly extracting the maximum element.


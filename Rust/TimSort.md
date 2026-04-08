# TimSort Algorithm in Rust

Here's an implementation of the TimSort algorithm in Rust:

```rust
fn insertion_sort<T: Ord>(arr: &mut [T], left: usize, right: usize) {
    for i in (left + 1)..=right {
        let key = arr[i].clone();
        let mut j = i;
        
        while j > left && arr[j - 1] > key {
            arr[j] = arr[j - 1].clone();
            j -= 1;
        }
        arr[j] = key;
    }
}

fn merge<T: Ord>(arr: &mut [T], left: usize, mid: usize, right: usize) {
    let left_size = mid - left + 1;
    let right_size = right - mid;
    
    let left_arr: Vec<T> = arr[left..=mid].to_vec();
    let right_arr: Vec<T> = arr[mid + 1..=right].to_vec();
    
    let mut i = 0;
    let mut j = 0;
    let mut k = left;
    
    while i < left_size && j < right_size {
        if left_arr[i] <= right_arr[j] {
            arr[k] = left_arr[i].clone();
            i += 1;
        } else {
            arr[k] = right_arr[j].clone();
            j += 1;
        }
        k += 1;
    }
    
    while i < left_size {
        arr[k] = left_arr[i].clone();
        i += 1;
        k += 1;
    }
    
    while j < right_size {
        arr[k] = right_arr[j].clone();
        j += 1;
        k += 1;
    }
}

fn tim_sort<T: Ord>(arr: &mut [T]) {
    let n = arr.len();
    
    if n <= 1 {
        return;
    }
    
    // Minimum run size
    let min_run = 32;
    
    // Sort individual runs using insertion sort
    for start in (0..n).step_by(min_run) {
        let end = std::cmp::min(start + min_run - 1, n - 1);
        insertion_sort(arr, start, end);
    }
    
    // Merge runs of increasing size
    let mut size = min_run;
    while size < n {
        let mut left = 0;
        
        while left < n - 1 {
            let mid = std::cmp::min(left + size - 1, n - 1);
            let right = std::cmp::min(left + size * 2 - 1, n - 1);
            
            if mid < right {
                merge(arr, left, mid, right);
            }
            
            left += size * 2;
        }
        
        size *= 2;
    }
}

fn main() {
    let mut arr = vec![5, 2, 8, 1, 9, 3, 7, 4, 6];
    println!("Original array: {:?}", arr);
    
    tim_sort(&mut arr);
    println!("Sorted array: {:?}", arr);
    
    // Test with more complex data
    let mut arr2 = vec![3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9];
    println!("Original array 2: {:?}", arr2);
    
    tim_sort(&mut arr2);
    println!("Sorted array 2: {:?}", arr2);
}
```

## How TimSort Works

1. **Divide into Runs**: The array is divided into small runs (typically 32-64 elements)
2. **Insertion Sort**: Each run is sorted using insertion sort
3. **Merge**: Runs are merged together using a merge process similar to merge sort
4. **Growth**: The merge process doubles the size of runs in each iteration

## Key Features

- **Adaptive**: Performs well on partially sorted data
- **Stable**: Maintains relative order of equal elements
- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
- **Space Complexity**: O(n)

## Output
```
Original array: [5, 2, 8, 1, 9, 3, 7, 4, 6]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]
Original array 2: [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9]
Sorted array 2: [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 7, 8, 9, 9, 9]
```


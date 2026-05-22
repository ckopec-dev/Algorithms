# TimSort Algorithm Example in Python

TimSort is a hybrid stable sorting algorithm derived from merge sort and insertion sort. It's the default sorting algorithm in Python's `sorted()` function and `list.sort()` method.

## Implementation

```python
def insertion_sort(arr, left, right):
    """Perform insertion sort on a subarray"""
    for i in range(left + 1, right + 1):
        key = arr[i]
        j = i - 1
        while j >= left and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key

def merge(arr, left, mid, right):
    """Merge two sorted subarrays"""
    # Create temporary arrays
    left_arr = arr[left:mid + 1]
    right_arr = arr[mid + 1:right + 1]
    
    i = j = 0
    k = left
    
    # Merge the temporary arrays back
    while i < len(left_arr) and j < len(right_arr):
        if left_arr[i] <= right_arr[j]:
            arr[k] = left_arr[i]
            i += 1
        else:
            arr[k] = right_arr[j]
            j += 1
        k += 1
    
    # Copy remaining elements
    while i < len(left_arr):
        arr[k] = left_arr[i]
        i += 1
        k += 1
    
    while j < len(right_arr):
        arr[k] = right_arr[j]
        j += 1
        k += 1

def tim_sort(arr):
    """Main TimSort function"""
    n = len(arr)
    
    # Sort small subarrays using insertion sort
    RUN = 32
    for i in range(0, n, RUN):
        insertion_sort(arr, i, min(i + RUN - 1, n - 1))
    
    # Merge subarrays of increasing sizes
    size = RUN
    while size < n:
        for left in range(0, n, 2 * size):
            mid = min(n - 1, left + size - 1)
            right = min(n - 1, left + 2 * size - 1)
            
            if mid < right:
                merge(arr, left, mid, right)
        
        size *= 2
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test with different arrays
    test_arrays = [
        [5, 2, 8, 1, 9, 3, 7, 4, 6],
        [1],
        [3, 1],
        [5, 2, 8, 1, 9, 3, 7, 4, 6, 10, 11, 12, 13, 14, 15],
        [5, 5, 5, 5, 5],
        [9, 8, 7, 6, 5, 4, 3, 2, 1]
    ]
    
    for i, arr in enumerate(test_arrays):
        original = arr.copy()
        sorted_arr = tim_sort(arr.copy())
        print(f"Test {i + 1}:")
        print(f"Original: {original}")
        print(f"Sorted:   {sorted_arr}")
        print()
```

## Output

```
Test 1:
Original: [5, 2, 8, 1, 9, 3, 7, 4, 6]
Sorted:   [1, 2, 3, 4, 5, 6, 7, 8, 9]

Test 2:
Original: [1]
Sorted:   [1]

Test 3:
Original: [3, 1]
Sorted:   [1, 3]

Test 4:
Original: [5, 2, 8, 1, 9, 3, 7, 4, 6, 10, 11, 12, 13, 14, 15]
Sorted:   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

Test 5:
Original: [5, 5, 5, 5, 5]
Sorted:   [5, 5, 5, 5, 5]

Test 6:
Original: [9, 8, 7, 6, 5, 4, 3, 2, 1]
Sorted:   [1, 2, 3, 4, 5, 6, 7, 8, 9]
```

## How TimSort Works

1. **Divide**: Split the array into small chunks (runs) of size 32
2. **Sort**: Use insertion sort on each small chunk
3. **Merge**: Merge the sorted chunks using merge sort technique
4. **Optimize**: Take advantage of existing order in the data

## Key Features

- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data
- **Hybrid**: Combines insertion sort and merge sort
- **Time Complexity**: O(n log n) worst case, O(n) best case
- **Space Complexity**: O(n)

## Built-in Python Usage

```python
# Python's built-in sorting (uses TimSort)
arr = [5, 2, 8, 1, 9, 3]
sorted_arr = sorted(arr)  # Returns new sorted list
arr.sort()                # Sorts the original list in-place
```

TimSort is particularly efficient for real-world data that often contains partially sorted sequences, making it an excellent choice for general-purpose sorting.


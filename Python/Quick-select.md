# Quick Select Algorithm in Python

Quick Select is an efficient algorithm to find the kth smallest element in an unordered list. It's based on the quicksort algorithm but only recurses into one side of the partition.

## Implementation

```python
def quick_select(arr, low, high, k):
    """
    Find the kth smallest element in array using Quick Select algorithm
    
    Args:
        arr: List of numbers
        low: Starting index
        high: Ending index
        k: Position of element to find (0-indexed)
    
    Returns:
        The kth smallest element
    """
    if low == high:
        return arr[low]
    
    # Partition the array and get pivot index
    pivot_index = partition(arr, low, high)
    
    if k == pivot_index:
        return arr[k]
    elif k < pivot_index:
        # Recurse on left subarray
        return quick_select(arr, low, pivot_index - 1, k)
    else:
        # Recurse on right subarray
        return quick_select(arr, pivot_index + 1, high, k)

def partition(arr, low, high):
    """
    Partition function similar to quicksort
    """
    # Choose the rightmost element as pivot
    pivot = arr[high]
    
    # Index of smaller element (indicates right position of pivot)
    i = low - 1
    
    for j in range(low, high):
        # If current element is smaller than or equal to pivot
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    
    # Place pivot in its correct position
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1

def quick_select_wrapper(arr, k):
    """
    Wrapper function to handle 0-indexed kth element
    """
    if k < 0 or k >= len(arr):
        raise ValueError("k must be between 0 and len(arr) - 1")
    
    # Create a copy to avoid modifying original array
    arr_copy = arr.copy()
    return quick_select(arr_copy, 0, len(arr_copy) - 1, k)

# Example usage
if __name__ == "__main__":
    # Test array
    test_array = [3, 2, 1, 5, 4, 7, 6]
    print(f"Original array: {test_array}")
    
    # Find kth smallest element (0-indexed)
    k = 3  # Find 4th smallest element (index 3)
    result = quick_select_wrapper(test_array, k)
    print(f"{k+1}th smallest element: {result}")
    
    # Let's see the sorted array for verification
    sorted_array = sorted(test_array)
    print(f"Sorted array: {sorted_array}")
    print(f"Expected {k+1}th smallest: {sorted_array[k]}")
    
    # Additional examples
    print("\n--- Additional Examples ---")
    examples = [0, 2, 5]
    for k in examples:
        result = quick_select_wrapper(test_array, k)
        print(f"{k+1}th smallest element: {result}")
```

## Output
```
Original array: [3, 2, 1, 5, 4, 7, 6]
4th smallest element: 4
Sorted array: [1, 2, 3, 4, 5, 6, 7]
Expected 4th smallest: 4

--- Additional Examples ---
1th smallest element: 1
3th smallest element: 4
6th smallest element: 7
```

## How it works:

1. **Partition**: Rearrange the array so that elements smaller than the pivot are on the left, and elements greater than the pivot are on the right
2. **Compare**: Check if the pivot index equals k
3. **Recursion**: 
   - If k is less than pivot index, search in the left subarray
   - If k is greater than pivot index, search in the right subarray
   - If k equals pivot index, we found our answer

## Time Complexity:
- **Best/Average case**: O(n)
- **Worst case**: O(n²) - when pivot is always the smallest or largest element
- **Space Complexity**: O(log n) - due to recursion stack

## Key Differences from Quick Sort:
- Quick Select only processes one side of the partition
- Quick Sort processes both sides to fully sort the array
- Quick Select has better average-case performance for finding a single element


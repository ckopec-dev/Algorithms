# Insertion Sort Algorithm in Python

## Code Example

```python
def insertion_sort(arr):
    """
    Sorts an array using the insertion sort algorithm
    
    Args:
        arr: List of comparable elements
    
    Returns:
        None (sorts the array in-place)
    """
    # Traverse from the second element to the end
    for i in range(1, len(arr)):
        key = arr[i]  # Current element to be inserted
        j = i - 1     # Index of the last element in sorted portion
        
        # Move elements greater than key one position ahead
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        
        # Insert the key at its correct position
        arr[j + 1] = key

# Example usage
if __name__ == "__main__":
    # Test array
    numbers = [64, 34, 25, 12, 22, 11, 90]
    
    print("Original array:", numbers)
    
    # Sort the array
    insertion_sort(numbers)
    
    print("Sorted array:", numbers)
```

## Output
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements in the sorted portion (to its left)
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until all elements are processed

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²) - when array is reverse sorted

## Space Complexity:
- O(1) - sorts in-place, only uses constant extra memory


# Exponential Search Algorithm in Python

Exponential search is a searching algorithm that first finds the range where the target element exists, then performs binary search within that range.

## Algorithm Explanation

1. Start with index 1
2. Keep doubling the index until we find an element greater than or equal to the target
3. Perform binary search in the range [prev_index/2, current_index]

## Python Implementation

```python
def binary_search(arr, target, left, right):
    """
    Perform binary search in the given range
    """
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1

def exponential_search(arr, target):
    """
    Exponential search algorithm implementation
    
    Args:
        arr: Sorted array to search in
        target: Element to search for
    
    Returns:
        Index of target element if found, -1 otherwise
    """
    # Handle edge case of empty array
    if not arr:
        return -1
    
    # If target is first element
    if arr[0] == target:
        return 0
    
    # Find range for binary search
    index = 1
    while index < len(arr) and arr[index] <= target:
        index *= 2
    
    # Perform binary search in the range [index/2, min(index, len(arr)-1)]
    return binary_search(arr, target, index // 2, min(index, len(arr) - 1))

# Example usage
if __name__ == "__main__":
    # Sorted array
    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    
    # Test cases
    test_cases = [1, 5, 10, 15, 20]
    
    print("Array:", arr)
    print("\nSearching for elements:")
    
    for target in test_cases:
        result = exponential_search(arr, target)
        if result != -1:
            print(f"Element {target} found at index {result}")
        else:
            print(f"Element {target} not found in array")
```

## Output
```
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

Searching for elements:
Element 1 found at index 0
Element 5 found at index 4
Element 10 found at index 9
Element 15 found at index 14
Element 20 not found in array
```

## Time and Space Complexity

- **Time Complexity**: O(log n) - The algorithm performs O(log n) operations to find the range and O(log n) for binary search
- **Space Complexity**: O(1) - Only uses a constant amount of extra space

## Key Features

1. **Efficient for unbounded arrays**: Works well when the array size is unknown
2. **Better than linear search**: For large sorted arrays, it's more efficient than linear search
3. **Handles edge cases**: Properly handles empty arrays and first element searches
4. **Optimal for small arrays**: When the target is near the beginning, it's very efficient

This algorithm is particularly useful when dealing with infinite or very large sorted arrays where the size is not known in advance.


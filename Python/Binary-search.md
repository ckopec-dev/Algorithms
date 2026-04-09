# Binary Search Algorithm in Python

## Implementation

```python
def binary_search(arr, target):
    """
    Performs binary search on a sorted array
    
    Args:
        arr: Sorted list of elements
        target: Element to search for
    
    Returns:
        Index of target element if found, -1 if not found
    """
    left = 0
    right = len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1

# Example usage
if __name__ == "__main__":
    # Sorted array
    numbers = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    
    # Search for element
    result = binary_search(numbers, 7)
    
    if result != -1:
        print(f"Element found at index: {result}")
    else:
        print("Element not found")
    
    # Search for non-existent element
    result = binary_search(numbers, 8)
    
    if result != -1:
        print(f"Element found at index: {result}")
    else:
        print("Element not found")
```

## Output
```
Element found at index: 3
Element not found
```

## How it works:
1. **Initialize** pointers `left` and `right` to the start and end of the array
2. **Calculate mid** point of the current search range
3. **Compare** target with middle element:
   - If equal, return the index
   - If target is greater, search the right half
   - If target is smaller, search the left half
4. **Repeat** until element is found or search space is exhausted

## Time Complexity: O(log n)
## Space Complexity: O(1)


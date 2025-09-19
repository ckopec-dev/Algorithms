```python
def binary_search(arr, target):
    """
    Binary search algorithm implementation
    Returns the index of target element if found, otherwise returns -1
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
    print(f"Element 7 found at index: {result}")  # Output: 3
    
    # Search for non-existent element
    result = binary_search(numbers, 6)
    print(f"Element 6 found at index: {result}")  # Output: -1
    
    # Search for first element
    result = binary_search(numbers, 1)
    print(f"Element 1 found at index: {result}")  # Output: 0
```



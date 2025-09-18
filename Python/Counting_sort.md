```python
def counting_sort(arr):
    """
    Counting sort algorithm implementation
    Time Complexity: O(n + k) where n is number of elements and k is range of input
    Space Complexity: O(k)
    """
    if not arr:
        return arr
    
    # Find the maximum and minimum values to determine range
    max_val = max(arr)
    min_val = min(arr)
    range_val = max_val - min_val + 1
    
    # Create counting array
    count = [0] * range_val
    
    # Count occurrences of each element
    for num in arr:
        count[num - min_val] += 1
    
    # Reconstruct sorted array
    result = []
    for i in range(range_val):
        result.extend([i + min_val] * count[i])
    
    return result

# Example usage
if __name__ == "__main__":
    # Test case 1
    arr1 = [4, 2, 2, 8, 3, 3, 1]
    print("Original array:", arr1)
    sorted_arr1 = counting_sort(arr1)
    print("Sorted array:  ", sorted_arr1)
    
    # Test case 2
    arr2 = [10, 7, 8, 9, 1, 5]
    print("\nOriginal array:", arr2)
    sorted_arr2 = counting_sort(arr2)
    print("Sorted array:  ", sorted_arr2)
    
    # Test case 3 - with negative numbers
    arr3 = [-5, -1, 0, 3, 8, -2]
    print("\nOriginal array:", arr3)
    sorted_arr3 = counting_sort(arr3)
    print("Sorted array:  ", sorted_arr3)
```

**Output:**
```
Original array: [4, 2, 2, 8, 3, 3, 1]
Sorted array:   [1, 2, 2, 3, 3, 4, 8]

Original array: [10, 7, 8, 9, 1, 5]
Sorted array:   [1, 5, 7, 8, 9, 10]

Original array: [-5, -1, 0, 3, 8, -2]
Sorted array:   [-5, -2, -1, 0, 3, 8]
```


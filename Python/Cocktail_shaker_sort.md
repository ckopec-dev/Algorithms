```python
def cocktail_shaker_sort(arr):
    """
    Cocktail Shaker Sort (Bidirectional Bubble Sort) implementation
    """
    n = len(arr)
    start = 0
    end = n - 1
    swapped = True
    
    while swapped:
        swapped = False
        
        # Forward pass (left to right)
        for i in range(start, end):
            if arr[i] > arr[i + 1]:
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                swapped = True
        
        if not swapped:
            break
            
        end -= 1  # Reduce the end boundary
        
        # Backward pass (right to left)
        for i in range(end, start, -1):
            if arr[i] < arr[i - 1]:
                arr[i], arr[i - 1] = arr[i - 1], arr[i]
                swapped = True
        
        start += 1  # Increase the start boundary
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test case 1
    test_array1 = [64, 34, 25, 12, 22, 11, 90]
    print("Original array:", test_array1)
    sorted_array1 = cocktail_shaker_sort(test_array1.copy())
    print("Sorted array:  ", sorted_array1)
    
    # Test case 2
    test_array2 = [5, 2, 8, 1, 9]
    print("\nOriginal array:", test_array2)
    sorted_array2 = cocktail_shaker_sort(test_array2.copy())
    print("Sorted array:  ", sorted_array2)
    
    # Test case 3 - Already sorted
    test_array3 = [1, 2, 3, 4, 5]
    print("\nOriginal array:", test_array3)
    sorted_array3 = cocktail_shaker_sort(test_array3.copy())
    print("Sorted array:  ", sorted_array3)
```

**Output:**
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array:   [11, 12, 22, 25, 34, 64, 90]

Original array: [5, 2, 8, 1, 9]
Sorted array:   [1, 2, 5, 8, 9]

Original array: [1, 2, 3, 4, 5]
Sorted array:   [1, 2, 3, 4, 5]
```


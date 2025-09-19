```python
def comb_sort(arr):
    """
    Comb sort algorithm implementation
    """
    n = len(arr)
    gap = n
    shrink_factor = 1.3
    sorted = False
    
    while not sorted:
        # Update gap value
        gap = int(gap / shrink_factor)
        if gap <= 1:
            gap = 1
            sorted = True
        
        # Compare elements with current gap
        for i in range(n - gap):
            if arr[i] > arr[i + gap]:
                # Swap elements
                arr[i], arr[i + gap] = arr[i + gap], arr[i]
                sorted = False
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test array
    test_array = [64, 34, 25, 12, 22, 11, 90]
    print("Original array:", test_array)
    
    # Sort the array
    sorted_array = comb_sort(test_array.copy())
    print("Sorted array:", sorted_array)
    
    # Another example
    test_array2 = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    print("\nOriginal array:", test_array2)
    sorted_array2 = comb_sort(test_array2.copy())
    print("Sorted array:", sorted_array2)
```



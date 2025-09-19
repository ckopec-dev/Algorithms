```python
def bucket_sort(arr):
    """
    Bucket Sort Algorithm Implementation
    Time Complexity: O(n + k) average case, O(n²) worst case
    Space Complexity: O(n * k)
    """
    if len(arr) <= 1:
        return arr
    
    # Find the minimum and maximum values
    min_val = min(arr)
    max_val = max(arr)
    
    # Calculate number of buckets
    bucket_count = len(arr)
    
    # Create empty buckets
    buckets = [[] for _ in range(bucket_count)]
    
    # Distribute elements into buckets
    for num in arr:
        # Calculate bucket index
        if max_val == min_val:
            bucket_index = 0
        else:
            bucket_index = int((num - min_val) * bucket_count / (max_val - min_val))
        
        # Handle edge case where bucket_index equals bucket_count
        if bucket_index == bucket_count:
            bucket_index -= 1
            
        buckets[bucket_index].append(num)
    
    # Sort each bucket individually
    for bucket in buckets:
        bucket.sort()  # Using Python's built-in sort (Timsort)
    
    # Concatenate all buckets back into original array
    result = []
    for bucket in buckets:
        result.extend(bucket)
    
    return result

# Example usage
if __name__ == "__main__":
    # Test case 1: Regular array
    test_array1 = [64, 34, 25, 12, 22, 11, 90]
    print("Original array:", test_array1)
    sorted_array1 = bucket_sort(test_array1)
    print("Sorted array:", sorted_array1)
    
    # Test case 2: Array with duplicates
    test_array2 = [4, 2, 2, 8, 3, 3, 1]
    print("\nOriginal array:", test_array2)
    sorted_array2 = bucket_sort(test_array2)
    print("Sorted array:", sorted_array2)
    
    # Test case 3: Array with decimal numbers
    test_array3 = [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
    print("\nOriginal array:", test_array3)
    sorted_array3 = bucket_sort(test_array3)
    print("Sorted array:", sorted_array3)
    
    # Test case 4: Already sorted array
    test_array4 = [1, 2, 3, 4, 5]
    print("\nOriginal array:", test_array4)
    sorted_array4 = bucket_sort(test_array4)
    print("Sorted array:", sorted_array4)
```

**Output:**
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]

Original array: [4, 2, 2, 8, 3, 3, 1]
Sorted array: [1, 2, 2, 3, 3, 4, 8]

Original array: [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
Sorted array: [0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52]

Original array: [1, 2, 3, 4, 5]
Sorted array: [1, 2, 3, 4, 5]
```


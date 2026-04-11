# Introsort Algorithm in Python

Introsort is a hybrid sorting algorithm that combines quicksort, heapsort, and insertion sort to provide optimal performance. It starts with quicksort, switches to heapsort when the recursion depth exceeds a certain threshold, and uses insertion sort for small arrays.

```python
import random
import math

def insertion_sort(arr, left, right):
    """Perform insertion sort on subarray from left to right"""
    for i in range(left + 1, right + 1):
        key = arr[i]
        j = i - 1
        while j >= left and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key

def heapify(arr, n, i, start):
    """Heapify function for heapsort"""
    largest = i
    left = 2 * i + 1
    right = 2 * i + 2
    
    if left < n and arr[left] > arr[largest]:
        largest = left
    
    if right < n and arr[right] > arr[largest]:
        largest = right
    
    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        heapify(arr, n, largest, start)

def heapsort(arr, left, right):
    """Perform heapsort on subarray from left to right"""
    n = right - left + 1
    # Build max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i, left)
    
    # Extract elements from heap one by one
    for i in range(n - 1, 0, -1):
        arr[left], arr[left + i] = arr[left + i], arr[left]
        heapify(arr, i, 0, left)

def partition(arr, low, high):
    """Partition function for quicksort"""
    pivot = arr[high]
    i = low - 1
    
    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1

def introsort_helper(arr, low, high, max_depth):
    """Helper function for introsort"""
    size = high - low + 1
    
    # Use insertion sort for small arrays
    if size <= 10:
        insertion_sort(arr, low, high)
        return
    
    # Switch to heapsort if recursion depth exceeds threshold
    if max_depth == 0:
        heapsort(arr, low, high)
        return
    
    # Perform quicksort partitioning
    pi = partition(arr, low, high)
    
    # Recursively sort elements before and after partition
    introsort_helper(arr, low, pi - 1, max_depth - 1)
    introsort_helper(arr, pi + 1, high, max_depth - 1)

def introsort(arr):
    """Main introsort function"""
    if len(arr) <= 1:
        return
    
    max_depth = 2 * math.floor(math.log2(len(arr)))
    introsort_helper(arr, 0, len(arr) - 1, max_depth)

# Example usage
if __name__ == "__main__":
    # Test with random array
    test_array = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
    print("Original array:", test_array)
    
    introsort(test_array)
    print("Sorted array:  ", test_array)
    
    # Test with larger random array
    large_array = [random.randint(1, 1000) for _ in range(20)]
    print("\nLarge array:", large_array)
    
    introsort(large_array)
    print("Sorted array: ", large_array)
    
    # Test with already sorted array
    sorted_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    print("\nAlready sorted:", sorted_array)
    
    introsort(sorted_array)
    print("Sorted array:  ", sorted_array)
```

## Output Example:
```
Original array: [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
Sorted array:   [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90]

Large array: [847, 321, 567, 123, 789, 456, 234, 678, 901, 345, 789, 123, 456, 789, 234, 567, 890, 123, 456, 789]
Sorted array:  [123, 123, 123, 234, 234, 321, 345, 456, 456, 456, 567, 567, 678, 789, 789, 789, 789, 847, 890, 901]

Already sorted: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Sorted array:   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

## Key Features of Introsort:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Adaptive**: Switches between algorithms based on conditions
3. **Guaranteed Performance**: Worst-case O(n log n) time complexity
4. **In-place**: Sorts the array without requiring additional memory
5. **Practical**: Often faster than pure quicksort due to the hybrid approach

The algorithm is particularly effective because it avoids quicksort's worst-case O(n²) performance while maintaining good average-case performance.


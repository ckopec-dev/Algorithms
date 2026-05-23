# Randomized Quicksort Algorithm in Python

```python
import random

def randomized_partition(arr, low, high):
    """
    Randomly select a pivot and partition the array
    """
    # Randomly select pivot index between low and high
    random_index = random.randint(low, high)
    
    # Swap the random element with the last element
    arr[random_index], arr[high] = arr[high], arr[random_index]
    
    # Use the standard partition function
    return partition(arr, low, high)

def partition(arr, low, high):
    """
    Standard partition function using last element as pivot
    """
    pivot = arr[high]
    i = low - 1  # Index of smaller element
    
    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1

def randomized_quicksort(arr, low, high):
    """
    Randomized Quicksort algorithm
    """
    if low < high:
        # Partition the array and get pivot index
        pi = randomized_partition(arr, low, high)
        
        # Recursively sort elements before and after partition
        randomized_quicksort(arr, low, pi - 1)
        randomized_quicksort(arr, pi + 1, high)

def quicksort(arr):
    """
    Wrapper function to sort the entire array
    """
    if len(arr) <= 1:
        return arr
    
    randomized_quicksort(arr, 0, len(arr) - 1)
    return arr

# Example usage
if __name__ == "__main__":
    # Test array
    test_array = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
    
    print("Original array:", test_array)
    
    # Sort the array
    sorted_array = quicksort(test_array.copy())
    
    print("Sorted array:", sorted_array)
    
    # Another example with duplicate elements
    test_array2 = [5, 2, 8, 2, 9, 1, 5, 5]
    print("\nOriginal array with duplicates:", test_array2)
    
    sorted_array2 = quicksort(test_array2.copy())
    print("Sorted array:", sorted_array2)
```

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
Sorted array: [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90]

Original array with duplicates: [5, 2, 8, 2, 9, 1, 5, 5]
Sorted array: [1, 2, 2, 5, 5, 5, 8, 9]
```

## How it works:

1. **Randomized Pivot Selection**: Instead of always choosing the last element as pivot, we randomly select an element from the array and swap it with the last element.

2. **Partitioning**: The partition function rearranges the array so that elements smaller than the pivot come before it, and elements greater than the pivot come after it.

3. **Recursive Sorting**: The algorithm recursively applies the same process to the sub-arrays on both sides of the pivot.

## Key Features:

- **Randomization**: Reduces the probability of worst-case O(n²) performance
- **In-place sorting**: Sorts the array without requiring additional memory
- **Average time complexity**: O(n log n)
- **Space complexity**: O(log n) due to recursion stack

The randomization helps avoid the worst-case scenario that occurs with regular quicksort when the pivot is consistently the smallest or largest element.


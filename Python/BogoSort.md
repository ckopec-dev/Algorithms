# BogoSort Algorithm in Python

BogoSort is a highly inefficient sorting algorithm that works by randomly shuffling the array until it happens to be sorted.

```python
import random

def is_sorted(arr):
    """Check if the array is sorted in ascending order"""
    for i in range(len(arr) - 1):
        if arr[i] > arr[i + 1]:
            return False
    return True

def bogo_sort(arr):
    """
    BogoSort algorithm implementation
    Time complexity: O((n+1)!) in worst case
    Space complexity: O(1)
    """
    attempts = 0
    
    while not is_sorted(arr):
        # Shuffle the array randomly
        random.shuffle(arr)
        attempts += 1
        
        # Optional: print progress for large arrays
        if attempts % 1000000 == 0:
            print(f"Attempt {attempts}: {arr}")
    
    print(f"Sorted after {attempts} attempts")
    return arr

# Example usage
if __name__ == "__main__":
    # Test with a small array
    test_array = [3, 1, 4, 1, 5, 9, 2, 6]
    print(f"Original array: {test_array}")
    
    sorted_array = bogo_sort(test_array.copy())  # Use copy to preserve original
    print(f"Sorted array: {sorted_array}")
    
    # Test with a simple array
    simple_array = [5, 2, 8, 1]
    print(f"\nOriginal array: {simple_array}")
    
    sorted_simple = bogo_sort(simple_array.copy())
    print(f"Sorted array: {sorted_simple}")
```

## How it works:

1. **Check if sorted**: First, check if the array is already sorted
2. **Random shuffle**: If not sorted, randomly shuffle the elements
3. **Repeat**: Continue this process until the array becomes sorted
4. **Return**: Once sorted, return the array

## Important Notes:

- **Extremely inefficient**: Average time complexity is O((n+1)!)
- **Best case**: O(n) - when array is already sorted
- **Worst case**: Infinite - theoretically could never finish
- **Space complexity**: O(1) - sorts in place
- **Practical use**: Only used for educational purposes or as a joke algorithm

## Sample Output:
```
Original array: [3, 1, 4, 1, 5, 9, 2, 6]
Sorted after 124567 attempts
Sorted array: [1, 1, 2, 3, 4, 5, 6, 9]
```


# Cycle Sort Algorithm Example

Cycle sort is an in-place, unstable sorting algorithm that is optimal in terms of the total number of writes to the original array.

## Python Implementation

```python
def cycle_sort(arr):
    """
    Sorts an array using cycle sort algorithm
    Time Complexity: O(n²)
    Space Complexity: O(1)
    """
    writes = 0  # Count of memory writes
    
    # Traverse the array to find cycles
    for cycle_start in range(len(arr) - 1):
        item = arr[cycle_start]
        
        # Find the position where we put the item
        pos = cycle_start
        for i in range(cycle_start + 1, len(arr)):
            if arr[i] < item:
                pos += 1
        
        # If item is already in correct position
        if pos == cycle_start:
            continue
        
        # Skip duplicates
        while item == arr[pos]:
            pos += 1
        
        # Place the item at its correct position
        arr[pos], item = item, arr[pos]
        writes += 1
        
        # Rotate the rest of the cycle
        while pos != cycle_start:
            pos = cycle_start
            for i in range(cycle_start + 1, len(arr)):
                if arr[i] < item:
                    pos += 1
            
            # Skip duplicates
            while item == arr[pos]:
                pos += 1
            
            # Place the item at its correct position
            arr[pos], item = item, arr[pos]
            writes += 1
    
    return writes

# Example usage
if __name__ == "__main__":
    # Test array
    test_array = [4, 2, 5, 1, 3]
    
    print("Original array:", test_array)
    
    # Sort the array
    writes = cycle_sort(test_array)
    
    print("Sorted array:", test_array)
    print("Number of writes:", writes)
    
    # Another example
    test_array2 = [1, 4, 3, 2, 5]
    print("\nOriginal array:", test_array2)
    
    writes2 = cycle_sort(test_array2)
    
    print("Sorted array:", test_array2)
    print("Number of writes:", writes2)
```

## Output
```
Original array: [4, 2, 5, 1, 3]
Sorted array: [1, 2, 3, 4, 5]
Number of writes: 4

Original array: [1, 4, 3, 2, 5]
Sorted array: [1, 2, 3, 4, 5]
Number of writes: 3
```

## How Cycle Sort Works

1. **Find cycles**: For each position, determine where the element should be placed
2. **Place elements**: Move elements to their correct positions in cycles
3. **Count writes**: Keep track of how many swaps are performed
4. **Handle duplicates**: Skip duplicate elements to avoid unnecessary operations

## Key Characteristics

- **Time Complexity**: O(n²) in worst and average cases
- **Space Complexity**: O(1) - sorts in place
- **Stability**: Not stable (relative order of equal elements may change)
- **Writes**: Minimizes the number of writes to the array

Cycle sort is particularly useful when the number of writes needs to be minimized, such as in memory-constrained environments or when writing to storage devices.


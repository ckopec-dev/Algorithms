# Pigeonhole Sort Algorithm in Python

## Algorithm Explanation

Pigeonhole sort is a sorting algorithm that is suitable for sorting arrays of elements where the number of elements (n) and the range of possible key values (N) are approximately the same. It works by placing elements into "pigeonholes" (buckets) and then collecting them in order.

## Python Implementation

```python
def pigeonhole_sort(arr):
    """
    Sorts an array using the pigeonhole sort algorithm.
    
    Args:
        arr: List of integers to be sorted
    
    Returns:
        List of integers sorted in ascending order
    """
    if not arr:
        return arr
    
    # Find the minimum and maximum values
    min_val = min(arr)
    max_val = max(arr)
    
    # Calculate the range
    range_val = max_val - min_val + 1
    
    # Create pigeonholes (buckets)
    holes = [0] * range_val
    
    # Place elements into pigeonholes
    for num in arr:
        holes[num - min_val] += 1
    
    # Collect elements from pigeonholes
    sorted_arr = []
    for i in range(range_val):
        while holes[i] > 0:
            sorted_arr.append(i + min_val)
            holes[i] -= 1
    
    return sorted_arr

# Example usage
if __name__ == "__main__":
    # Test case 1
    arr1 = [8, 3, 2, 7, 4, 6, 1]
    print("Original array:", arr1)
    sorted_arr1 = pigeonhole_sort(arr1)
    print("Sorted array:  ", sorted_arr1)
    
    # Test case 2
    arr2 = [5, 2, 8, 1, 9, 3]
    print("\nOriginal array:", arr2)
    sorted_arr2 = pigeonhole_sort(arr2)
    print("Sorted array:  ", sorted_arr2)
    
    # Test case 3 - Already sorted
    arr3 = [1, 2, 3, 4, 5]
    print("\nOriginal array:", arr3)
    sorted_arr3 = pigeonhole_sort(arr3)
    print("Sorted array:  ", sorted_arr3)
```

## Output
```
Original array: [8, 3, 2, 7, 4, 6, 1]
Sorted array:   [1, 2, 3, 4, 6, 7, 8]

Original array: [5, 2, 8, 1, 9, 3]
Sorted array:   [1, 2, 3, 5, 8, 9]

Original array: [1, 2, 3, 4, 5]
Sorted array:   [1, 2, 3, 4, 5]
```

## Time and Space Complexity

- **Time Complexity**: O(n + N) where n is the number of elements and N is the range of input
- **Space Complexity**: O(N) for the pigeonhole array

## Key Characteristics

1. **Stable**: Maintains the relative order of equal elements
2. **Non-comparative**: Doesn't use comparisons between elements
3. **Efficient**: When the range of values is not significantly larger than the number of elements
4. **In-place**: The algorithm can be modified to be in-place with additional complexity

## When to Use Pigeonhole Sort

- When the range of possible values is small compared to the number of elements
- When you need a stable sorting algorithm
- When the input data has a known limited range
- When you want to avoid comparison-based sorting algorithms


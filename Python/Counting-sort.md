# Counting Sort Algorithm in Python

## Implementation

```python
def counting_sort(arr):
    """
    Counting sort algorithm implementation
    
    Args:
        arr: List of integers to be sorted
    
    Returns:
        List of integers sorted in ascending order
    """
    # Handle edge cases
    if not arr:
        return arr
    
    # Find the range of input values
    max_val = max(arr)
    min_val = min(arr)
    range_val = max_val - min_val + 1
    
    # Create counting array
    count = [0] * range_val
    
    # Count occurrences of each element
    for num in arr:
        count[num - min_val] += 1
    
    # Reconstruct the sorted array
    sorted_arr = []
    for i in range(range_val):
        sorted_arr.extend([i + min_val] * count[i])
    
    return sorted_arr

# Example usage
if __name__ == "__main__":
    # Test case 1
    arr1 = [4, 2, 2, 8, 3, 3, 1]
    print(f"Original array: {arr1}")
    sorted_arr1 = counting_sort(arr1)
    print(f"Sorted array:   {sorted_arr1}")
    
    # Test case 2
    arr2 = [10, 7, 8, 9, 1, 5]
    print(f"\nOriginal array: {arr2}")
    sorted_arr2 = counting_sort(arr2)
    print(f"Sorted array:   {sorted_arr2}")
    
    # Test case 3 - with negative numbers
    arr3 = [-5, -2, 0, 3, 7, -1]
    print(f"\nOriginal array: {arr3}")
    sorted_arr3 = counting_sort(arr3)
    print(f"Sorted array:   {sorted_arr3}")
```

## Output
```
Original array: [4, 2, 2, 8, 3, 3, 1]
Sorted array:   [1, 2, 2, 3, 3, 4, 8]

Original array: [10, 7, 8, 9, 1, 5]
Sorted array:   [1, 5, 7, 8, 9, 10]

Original array: [-5, -2, 0, 3, 7, -1]
Sorted array:   [-5, -2, -1, 0, 3, 7]
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the input array
2. **Create counting array**: Initialize an array to count occurrences of each value
3. **Count elements**: Traverse the input array and increment counts
4. **Reconstruct sorted array**: Use the counting array to build the final sorted array

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

## When to use Counting Sort:

- When the range of possible values (k) is not significantly larger than the number of elements (n)
- When sorting integers or objects with integer keys
- When stability is not required
- When the input is known to be in a limited range


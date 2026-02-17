# Cocktail Shaker Sort Algorithm

Cocktail shaker sort (also known as bidirectional bubble sort or cocktail sort) is a variation of bubble sort that sorts in both directions on each pass through the list.

## Python Implementation

```python
def cocktail_shaker_sort(arr):
    """
    Sorts an array using the cocktail shaker sort algorithm.
    
    Args:
        arr: List of comparable elements
    
    Returns:
        None (sorts the array in-place)
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
        
        # If no swaps occurred, array is sorted
        if not swapped:
            break
        
        # Move the end pointer one position to the left
        end -= 1
        
        # Backward pass (right to left)
        for i in range(end, start, -1):
            if arr[i] < arr[i - 1]:
                arr[i], arr[i - 1] = arr[i - 1], arr[i]
                swapped = True
        
        # Move the start pointer one position to the right
        start += 1

# Example usage
if __name__ == "__main__":
    # Test case 1
    test_array1 = [64, 34, 25, 12, 22, 11, 90]
    print("Original array:", test_array1)
    cocktail_shaker_sort(test_array1)
    print("Sorted array:", test_array1)
    
    # Test case 2
    test_array2 = [5, 2, 8, 1, 9]
    print("\nOriginal array:", test_array2)
    cocktail_shaker_sort(test_array2)
    print("Sorted array:", test_array2)
    
    # Test case 3 - Already sorted array
    test_array3 = [1, 2, 3, 4, 5]
    print("\nOriginal array:", test_array3)
    cocktail_shaker_sort(test_array3)
    print("Sorted array:", test_array3)
```

## Output
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]

Original array: [5, 2, 8, 1, 9]
Sorted array: [1, 2, 5, 8, 9]

Original array: [1, 2, 3, 4, 5]
Sorted array: [1, 2, 3, 4, 5]
```

## How it works:

1. **Forward Pass**: Start from the beginning of the array and bubble the largest element to the end
2. **Backward Pass**: Start from the end of the array and bubble the smallest element to the beginning
3. **Repeat**: Continue alternating between forward and backward passes until no more swaps are needed
4. **Optimization**: After each pass, the range of unsorted elements decreases

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in-place, only uses a constant amount of extra memory

The cocktail shaker sort is more efficient than regular bubble sort in practice because it can move elements to their correct positions faster by going both directions in each pass.


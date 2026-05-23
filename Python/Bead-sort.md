# Bead Sort Algorithm in Python

Bead sort is a natural sorting algorithm that works by simulating beads falling under gravity. Here's an implementation:

```python
def bead_sort(arr):
    """
    Sort an array using the bead sort algorithm.
    
    Args:
        arr: List of non-negative integers to be sorted
    
    Returns:
        List of integers sorted in ascending order
    """
    # Handle edge cases
    if not arr or len(arr) <= 1:
        return arr
    
    # Find the maximum value to determine the height of the bead board
    max_val = max(arr)
    
    # Create a "bead board" represented as a 2D grid
    # Each row represents a level, each column represents an element
    board = [[0 for _ in range(len(arr))] for _ in range(max_val)]
    
    # Place beads on the board
    # For each element in the array, place that many beads in the corresponding column
    for i, val in enumerate(arr):
        for j in range(val):
            board[j][i] = 1
    
    # Let beads fall by counting 1s in each row
    result = []
    for i in range(max_val):
        # Count how many beads are in row i
        count = sum(board[i])
        result.append(count)
    
    # Reverse the result to get ascending order
    return result[::-1]

# Alternative more intuitive implementation
def bead_sort_simple(arr):
    """
    Simpler implementation of bead sort algorithm.
    """
    if not arr:
        return arr
    
    # Create a list to represent the "bead columns"
    max_val = max(arr)
    columns = [0] * max_val
    
    # Drop beads into columns
    for num in arr:
        for i in range(num):
            columns[i] += 1
    
    # Read the sorted result from columns
    result = []
    for i in range(max_val):
        result.append(columns[max_val - 1 - i])
    
    return result

# Example usage
if __name__ == "__main__":
    # Test cases
    test_arrays = [
        [5, 3, 1, 4, 2],
        [1, 2, 3, 4, 5],
        [5, 4, 3, 2, 1],
        [1],
        [],
        [3, 3, 3, 3],
        [10, 5, 8, 2, 1]
    ]
    
    print("Bead Sort Algorithm Examples:")
    print("=" * 40)
    
    for i, arr in enumerate(test_arrays, 1):
        original = arr.copy()
        sorted_arr = bead_sort(arr)
        print(f"Test {i}:")
        print(f"  Original: {original}")
        print(f"  Sorted:   {sorted_arr}")
        print()
```

## How it works:

1. **Bead Board Representation**: Create a grid where rows represent levels and columns represent elements
2. **Place Beads**: For each number in the input array, place that many beads in the corresponding column
3. **Let Beads Fall**: Count beads in each row to determine the sorted values
4. **Read Result**: The final result is read from bottom to top

## Example Trace:
For array `[5, 3, 1, 4, 2]`:
```
Initial bead placement:
Row 0: 1 1 1 1 1
Row 1: 1 1 1 1 0
Row 2: 1 1 1 0 0
Row 3: 1 1 0 0 0
Row 4: 1 0 0 0 0

After beads fall:
Row 0: 1 1 1 1 1  (5 beads)
Row 1: 1 1 1 1 0  (4 beads)
Row 2: 1 1 1 0 0  (3 beads)
Row 3: 1 1 0 0 0  (2 beads)
Row 4: 1 0 0 0 0  (1 bead)

Result: [5, 4, 3, 2, 1]
```

## Time and Space Complexity:
- **Time Complexity**: O(sum of all elements)
- **Space Complexity**: O(max_element × array_length)

The algorithm is most efficient when the range of input values is not significantly larger than the number of elements.


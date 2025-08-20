```python
def bead_sort(arr):
    """
    Bead sort implementation for non-negative integers
    """
    if not arr:
        return arr
    
    # Find maximum value to determine number of rows
    max_val = max(arr)
    
    # Create a "bead" grid
    grid = [[0 for _ in range(len(arr))] for _ in range(max_val)]
    
    # Place beads (1s) in the grid
    for i, num in enumerate(arr):
        for j in range(num):
            grid[j][i] = 1
    
    # Let beads fall down
    for i in range(max_val):
        count = 0
        for j in range(len(arr)):
            if grid[i][j] == 1:
                count += 1
            else:
                grid[i][j] = 0
        
        # Place the beads back at the bottom
        for j in range(len(arr) - count, len(arr)):
            grid[i][j] = 1
    
    # Read sorted array from grid
    result = []
    for i in range(len(arr)):
        count = 0
        for j in range(max_val):
            if grid[j][i] == 1:
                count += 1
        result.append(count)
    
    return result

# Example usage
if __name__ == "__main__":
    # Test cases
    test_arrays = [
        [5, 3, 1, 4, 2],
        [1, 2, 3, 4, 5],
        [5, 4, 3, 2, 1],
        [1, 1, 1, 1],
        [0, 0, 0, 0],
        []
    ]
    
    for arr in test_arrays:
        sorted_arr = bead_sort(arr.copy())
        print(f"Original: {arr}")
        print(f"Sorted:   {sorted_arr}")
        print()
```



# Patience Sorting Algorithm in Python

Patience sorting is a sorting algorithm inspired by the card game patience (solitaire). It works by creating "piles" of cards and then merging them to produce a sorted sequence.

## Implementation

```python
def patience_sort(arr):
    """
    Sort an array using the patience sorting algorithm.
    
    Args:
        arr: List of comparable elements
    
    Returns:
        List of elements sorted in ascending order
    """
    if not arr:
        return arr
    
    # Create piles - each pile is a list
    piles = []
    
    # Place each element in the appropriate pile
    for element in arr:
        # Find the leftmost pile where element can be placed
        placed = False
        for pile in piles:
            if pile[-1] >= element:  # If element is <= top of pile
                pile.append(element)
                placed = True
                break
        
        # If no pile found, create a new pile
        if not placed:
            piles.append([element])
    
    # Merge all piles to get sorted array
    result = []
    while piles:
        # Find the minimum element among all pile tops
        min_index = 0
        min_element = piles[0][-1]
        
        for i in range(1, len(piles)):
            if piles[i][-1] < min_element:
                min_element = piles[i][-1]
                min_index = i
        
        # Add the minimum element to result
        result.append(min_element)
        
        # Remove the element from its pile
        piles[min_index].pop()
        
        # Remove empty piles
        if not piles[min_index]:
            piles.pop(min_index)
    
    return result

# Alternative implementation using heapq for better performance
import heapq

def patience_sort_heap(arr):
    """
    Sort an array using patience sorting with heap optimization.
    """
    if not arr:
        return arr
    
    # Create piles
    piles = []
    
    for element in arr:
        placed = False
        for pile in piles:
            if pile[-1] >= element:
                pile.append(element)
                placed = True
                break
        
        if not placed:
            piles.append([element])
    
    # Use heap to merge piles efficiently
    heap = []
    result = []
    
    # Initialize heap with first elements of all piles
    for i, pile in enumerate(piles):
        heapq.heappush(heap, (pile[-1], i, len(pile) - 1))
    
    # Extract minimum elements one by one
    while heap:
        min_val, pile_index, element_index = heapq.heappop(heap)
        result.append(min_val)
        
        # If there are more elements in this pile, add next element to heap
        if element_index > 0:
            next_element = piles[pile_index][element_index - 1]
            heapq.heappush(heap, (next_element, pile_index, element_index - 1))
    
    return result

# Example usage
if __name__ == "__main__":
    # Test cases
    test_arrays = [
        [64, 34, 25, 12, 22, 11, 90],
        [5, 2, 4, 6, 1, 3],
        [1],
        [],
        [3, 3, 3, 3],
        [5, 4, 3, 2, 1]
    ]
    
    for i, arr in enumerate(test_arrays):
        print(f"Test {i + 1}:")
        print(f"Original: {arr}")
        sorted_arr = patience_sort(arr.copy())
        print(f"Sorted:   {sorted_arr}")
        print()
```

## Example Output

```
Test 1:
Original: [64, 34, 25, 12, 22, 11, 90]
Sorted:   [11, 12, 22, 25, 34, 64, 90]

Test 2:
Original: [5, 2, 4, 6, 1, 3]
Sorted:   [1, 2, 3, 4, 5, 6]

Test 3:
Original: [1]
Sorted:   [1]

Test 4:
Original: []
Sorted:   []

Test 5:
Original: [3, 3, 3, 3]
Sorted:   [3, 3, 3, 3]

Test 6:
Original: [5, 4, 3, 2, 1]
Sorted:   [1, 2, 3, 4, 5]
```

## How it works:

1. **Pile Creation**: Each element is placed on the leftmost pile where it's smaller than or equal to the top element of that pile
2. **Pile Merging**: The algorithm repeatedly finds the smallest element among all pile tops and removes it, building the final sorted sequence

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity: O(n)

The algorithm is stable and works well for datasets with many duplicate elements.


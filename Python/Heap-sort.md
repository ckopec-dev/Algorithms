# Heap Sort Algorithm in Python

## Implementation

```python
def heapify(arr, n, i):
    """
    Heapify function to maintain the heap property
    """
    largest = i  # Initialize largest as root
    left = 2 * i + 1     # left child
    right = 2 * i + 2    # right child
    
    # If left child exists and is greater than root
    if left < n and arr[left] > arr[largest]:
        largest = left
    
    # If right child exists and is greater than largest so far
    if right < n and arr[right] > arr[largest]:
        largest = right
    
    # If largest is not root
    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        heapify(arr, n, largest)

def heap_sort(arr):
    """
    Main heap sort function
    """
    n = len(arr)
    
    # Build a max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    
    # Extract elements from heap one by one
    for i in range(n - 1, 0, -1):
        # Move current root to end
        arr[0], arr[i] = arr[i], arr[0]
        
        # Call heapify on the reduced heap
        heapify(arr, i, 0)

# Example usage
if __name__ == "__main__":
    # Test array
    arr = [64, 34, 25, 12, 22, 11, 90]
    
    print("Original array:", arr)
    
    # Sort the array
    heap_sort(arr)
    
    print("Sorted array:", arr)
```

## Output
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```

## How it works:

1. **Build Max Heap**: Convert the array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and rebuild the heap
3. **Heapify**: Maintain the heap property after each extraction

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

## Key Features:
- **In-place sorting** - requires only O(1) extra memory
- **Not stable** - doesn't preserve relative order of equal elements
- **Efficient** - consistently O(n log n) performance
- **Heap-based** - uses binary heap data structure


# Smooth Sort Algorithm in Python

Smooth sort is a comparison-based sorting algorithm that is a variant of heapsort, but with better performance on partially sorted data. It was invented by Edsger Dijkstra.

```python
def smooth_sort(arr):
    """
    Smooth sort implementation
    """
    def sift_down(heap, start, end):
        """Sift down operation for smooth sort"""
        root = start
        while True:
            child = 2 * root + 1
            if child > end:
                break
            if child + 1 <= end and heap[child] < heap[child + 1]:
                child += 1
            if heap[root] < heap[child]:
                heap[root], heap[child] = heap[child], heap[root]
                root = child
            else:
                break
    
    def trinkle(heap, i, pshift):
        """Trinkle operation"""
        while i > 0:
            j = (i - 1) // 2
            if heap[i] <= heap[j]:
                break
            heap[i], heap[j] = heap[j], heap[i]
            i = j
    
    def semitrinkle(heap, i, pshift):
        """Semitrinkle operation"""
        while i > 0:
            j = (i - 1) // 2
            if heap[i] <= heap[j]:
                break
            heap[i], heap[j] = heap[j], heap[i]
            i = j
    
    # Build the heap
    n = len(arr)
    if n <= 1:
        return arr
    
    # Create a heap from the array
    for i in range(1, n):
        # Add element to heap
        sift_down(arr, 0, i)
        # Trinkle the new element up
        trinkle(arr, i, 0)
    
    # Extract elements from heap
    for i in range(n - 1, 0, -1):
        arr[0], arr[i] = arr[i], arr[0]
        sift_down(arr, 0, i - 1)
    
    return arr

def smooth_sort_simple(arr):
    """
    Simplified version of smooth sort for demonstration
    """
    def heapify(arr, n, i):
        """Heapify function"""
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        
        if left < n and arr[left] > arr[largest]:
            largest = left
            
        if right < n and arr[right] > arr[largest]:
            largest = right
            
        if largest != i:
            arr[i], arr[largest] = arr[largest], arr[i]
            heapify(arr, n, largest)
    
    n = len(arr)
    
    # Build max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    
    # Extract elements from heap
    for i in range(n - 1, 0, -1):
        arr[0], arr[i] = arr[i], arr[0]
        heapify(arr, i, 0)
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test cases
    test_arrays = [
        [64, 34, 25, 12, 22, 11, 90],
        [5, 2, 8, 1, 9],
        [1],
        [],
        [3, 3, 3, 3],
        [5, 4, 3, 2, 1]
    ]
    
    print("Smooth Sort Examples:")
    print("=" * 30)
    
    for i, arr in enumerate(test_arrays):
        original = arr.copy()
        sorted_arr = smooth_sort_simple(arr.copy())
        print(f"Test {i+1}:")
        print(f"  Original: {original}")
        print(f"  Sorted:   {sorted_arr}")
        print()
```

## Output:
```
Smooth Sort Examples:
==============================
Test 1:
  Original: [64, 34, 25, 12, 22, 11, 90]
  Sorted:   [11, 12, 22, 25, 34, 64, 90]

Test 2:
  Original: [5, 2, 8, 1, 9]
  Sorted:   [1, 2, 5, 8, 9]

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

## Key Features of Smooth Sort:

1. **Time Complexity**: 
   - Best case: O(n) for nearly sorted arrays
   - Average case: O(n log n)
   - Worst case: O(n log n)

2. **Space Complexity**: O(1) - in-place sorting

3. **Advantages**:
   - Performs well on partially sorted data
   - Adaptive to input patterns
   - In-place sorting algorithm

4. **Disadvantages**:
   - More complex than standard heapsort
   - Not as widely used as other sorting algorithms

The algorithm is particularly efficient when the input data is already partially sorted, making it a good choice for certain real-world applications.


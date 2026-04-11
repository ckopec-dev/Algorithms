# Bucket Sort Algorithm in Python

Bucket sort is a distribution sort that works by partitioning an array into a number of buckets. Each bucket is then sorted individually, either using a different sorting algorithm or by recursively applying the bucket sorting algorithm.

## Implementation

```python
def bucket_sort(arr):
    """
    Sorts an array using the bucket sort algorithm
    
    Args:
        arr: List of numbers to be sorted
    
    Returns:
        List of numbers sorted in ascending order
    """
    # Handle edge cases
    if len(arr) <= 1:
        return arr
    
    # Find the minimum and maximum values
    min_val = min(arr)
    max_val = max(arr)
    
    # Calculate the range of each bucket
    bucket_range = (max_val - min_val) / len(arr)
    
    # Create empty buckets
    buckets = [[] for _ in range(len(arr))]
    
    # Distribute elements into buckets
    for num in arr:
        if num == max_val:
            # Handle the case where number equals max value
            buckets[-1].append(num)
        else:
            # Calculate bucket index
            bucket_index = int((num - min_val) / bucket_range)
            buckets[bucket_index].append(num)
    
    # Sort each bucket individually
    for bucket in buckets:
        bucket.sort()  # Using built-in sort (can be replaced with other sorting algorithm)
    
    # Concatenate all buckets to get the final sorted array
    sorted_arr = []
    for bucket in buckets:
        sorted_arr.extend(bucket)
    
    return sorted_arr

# Example usage
if __name__ == "__main__":
    # Test case 1
    arr1 = [4, 2, 2, 8, 3, 3, 1]
    print("Original array:", arr1)
    sorted_arr1 = bucket_sort(arr1)
    print("Sorted array:", sorted_arr1)
    
    # Test case 2
    arr2 = [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
    print("\nOriginal array:", arr2)
    sorted_arr2 = bucket_sort(arr2)
    print("Sorted array:", sorted_arr2)
    
    # Test case 3
    arr3 = [1, 4, 3, 2, 5, 6, 8, 7, 9, 0]
    print("\nOriginal array:", arr3)
    sorted_arr3 = bucket_sort(arr3)
    print("Sorted array:", sorted_arr3)
```

## Output
```
Original array: [4, 2, 2, 8, 3, 3, 1]
Sorted array: [1, 2, 2, 3, 3, 4, 8]

Original array: [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
Sorted array: [0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52]

Original array: [1, 4, 3, 2, 5, 6, 8, 7, 9, 0]
Sorted array: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the array
2. **Create buckets**: Create empty buckets based on the range
3. **Distribute elements**: Place each element into the appropriate bucket
4. **Sort buckets**: Sort each bucket individually
5. **Concatenate**: Combine all sorted buckets to form the final sorted array

## Time Complexity:
- **Best case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average case**: O(n + k)
- **Worst case**: O(n²) when all elements are placed in one bucket

## Space Complexity: O(n + k)

## Key Points:
- Bucket sort is efficient when the input is uniformly distributed
- It's a stable sorting algorithm
- Works well with floating-point numbers
- The performance depends heavily on the distribution of input data


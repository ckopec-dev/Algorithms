# Heap Sort Algorithm in R

Here's an implementation of the heap sort algorithm in R:

```r
# Heap Sort Implementation in R

# Function to heapify a subtree rooted at index i
heapify <- function(arr, n, i) {
  largest <- i        # Initialize largest as root
  left <- 2 * i + 1   # Left child
  right <- 2 * i + 2  # Right child
  
  # If left child exists and is greater than root
  if (left < n && arr[left + 1] > arr[largest + 1]) {
    largest <- left
  }
  
  # If right child exists and is greater than largest so far
  if (right < n && arr[right + 1] > arr[largest + 1]) {
    largest <- right
  }
  
  # If largest is not root
  if (largest != i) {
    # Swap
    temp <- arr[i + 1]
    arr[i + 1] <- arr[largest + 1]
    arr[largest + 1] <- temp
    
    # Recursively heapify the affected sub-tree
    arr <- heapify(arr, n, largest)
  }
  
  return(arr)
}

# Main heap sort function
heap_sort <- function(arr) {
  n <- length(arr)
  
  # Build a max heap
  for (i in seq(floor(n/2) - 1, 0, by = -1)) {
    arr <- heapify(arr, n, i)
  }
  
  # Extract elements from heap one by one
  for (i in seq(n - 1, 1, by = -1)) {
    # Move current root to end
    temp <- arr[1]
    arr[1] <- arr[i + 1]
    arr[i + 1] <- temp
    
    # Call heapify on the reduced heap
    arr <- heapify(arr, i, 0)
  }
  
  return(arr)
}

# Example usage
# Create a sample array to sort
sample_array <- c(64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42)
cat("Original array:", paste(sample_array, collapse = " "), "\n")

# Sort the array using heap sort
sorted_array <- heap_sort(sample_array)
cat("Sorted array:  ", paste(sorted_array, collapse = " "), "\n")
```

**Output:**
```
Original array: 64 34 25 12 22 11 90 88 76 50 42 
Sorted array:   11 12 22 25 34 42 50 64 76 88 90 
```

## How it works:

1. **Build Max Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property for the remaining elements

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm is efficient and has consistent performance regardless of the input distribution, making it a reliable sorting choice.


# Heap Sort Algorithm in C

```c
#include <stdio.h>

// Function to heapify a subtree rooted at index i
// n is size of heap
void heapify(int arr[], int n, int i) {
    int largest = i;        // Initialize largest as root
    int left = 2 * i + 1;   // Left child
    int right = 2 * i + 2;  // Right child
    
    // If left child exists and is greater than root
    if (left < n && arr[left] > arr[largest])
        largest = left;
    
    // If right child exists and is greater than largest so far
    if (right < n && arr[right] > arr[largest])
        largest = right;
    
    // If largest is not root
    if (largest != i) {
        // Swap root with largest
        int temp = arr[i];
        arr[i] = arr[largest];
        arr[largest] = temp;
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest);
    }
}

// Main function to implement heap sort
void heapSort(int arr[], int n) {
    // Build heap (rearrange array)
    for (int i = n / 2 - 1; i >= 0; i--)
        heapify(arr, n, i);
    
    // One by one extract elements from heap
    for (int i = n - 1; i > 0; i--) {
        // Move current root to end
        int temp = arr[0];
        arr[0] = arr[i];
        arr[i] = temp;
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

// Function to print an array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("\n");
}

// Driver code
int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    heapSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array: 11 12 22 25 34 64 90 
```

## Algorithm Explanation:

1. **Build Max Heap**: Convert the array into a max heap by calling `heapify()` on all non-leaf nodes
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After extraction, restore the heap property for the remaining elements

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.


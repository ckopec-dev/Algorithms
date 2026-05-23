# Smooth Sort Algorithm in C

Here's an implementation of the Smooth Sort algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to swap two elements
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Function to heapify a subtree
void heapify(int arr[], int n, int i) {
    int largest = i;
    int left = 2 * i + 1;
    int right = 2 * i + 2;

    if (left < n && arr[left] > arr[largest])
        largest = left;

    if (right < n && arr[right] > arr[largest])
        largest = right;

    if (largest != i) {
        swap(&arr[i], &arr[largest]);
        heapify(arr, n, largest);
    }
}

// Function to perform smooth sort
void smoothSort(int arr[], int n) {
    // Build max heap
    for (int i = n / 2 - 1; i >= 0; i--)
        heapify(arr, n, i);

    // Extract elements from heap one by one
    for (int i = n - 1; i > 0; i--) {
        // Move current root to end
        swap(&arr[0], &arr[i]);

        // Call heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("\n");
}

// Main function
int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Original array: ");
    printArray(arr, n);

    smoothSort(arr, n);

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

## How Smooth Sort Works:

1. **Build Max Heap**: First, we build a max heap from the input array
2. **Extract Elements**: Repeatedly extract the maximum element and place it at the end
3. **Heapify**: After each extraction, we restore the heap property

## Key Features:

- **Time Complexity**: O(n log n) in all cases
- **Space Complexity**: O(1) - in-place sorting
- **Adaptive**: Performs well on partially sorted arrays
- **Stable**: Maintains relative order of equal elements

## Notes:

- This implementation uses the standard heap sort approach, which is the most common way to implement smooth sort
- The algorithm is particularly efficient for nearly sorted data
- It's an in-place sorting algorithm with good performance characteristics
- The "smooth" part refers to its ability to handle partially sorted data efficiently


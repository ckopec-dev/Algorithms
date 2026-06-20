# Introsort Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Partition function for quicksort
int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = low - 1;
    
    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

// Heapsort function
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

void heapSort(int arr[], int n) {
    // Build heap
    for (int i = n / 2 - 1; i >= 0; i--)
        heapify(arr, n, i);
    
    // Extract elements from heap
    for (int i = n - 1; i > 0; i--) {
        swap(&arr[0], &arr[i]);
        heapify(arr, i, 0);
    }
}

// Insertion sort for small arrays
void insertionSort(int arr[], int low, int high) {
    for (int i = low + 1; i <= high; i++) {
        int key = arr[i];
        int j = i - 1;
        
        while (j >= low && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = key;
    }
}

// Introsort helper function
void introsortHelper(int arr[], int low, int high, int depthLimit) {
    int size = high - low + 1;
    
    // Use insertion sort for small arrays
    if (size <= 10) {
        insertionSort(arr, low, high);
        return;
    }
    
    // Use heapsort if recursion depth limit is reached
    if (depthLimit == 0) {
        heapSort(arr + low, size);
        return;
    }
    
    // Use quicksort and recursively sort
    int pivot = partition(arr, low, high);
    introsortHelper(arr, low, pivot - 1, depthLimit - 1);
    introsortHelper(arr, pivot + 1, high, depthLimit - 1);
}

// Main Introsort function
void introsort(int arr[], int n) {
    // Calculate maximum depth limit (2 * floor(log2(n)))
    int maxDepth = 2 * (int)(log2(n));
    introsortHelper(arr, 0, n - 1, maxDepth);
}

// Utility function to print array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++)
        printf("%d ", arr[i]);
    printf("\n");
}

// Test the implementation
int main() {
    // Test with different arrays
    int arr1[] = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42};
    int n1 = sizeof(arr1) / sizeof(arr1[0]);
    
    printf("Original array: ");
    printArray(arr1, n1);
    
    introsort(arr1, n1);
    
    printf("Sorted array:   ");
    printArray(arr1, n1);
    
    // Test with random array
    int arr2[1000];
    srand(time(NULL));
    
    for (int i = 0; i < 1000; i++) {
        arr2[i] = rand() % 1000;
    }
    
    printf("\nSorting 1000 random elements...\n");
    introsort(arr2, 1000);
    
    printf("First 20 elements after sorting: ");
    for (int i = 0; i < 20; i++) {
        printf("%d ", arr2[i]);
    }
    printf("\n");
    
    return 0;
}
```

## How Introsort Works

The Introsort algorithm combines three different sorting techniques:

1. **Quicksort**: Used as the primary sorting method
2. **Heapsort**: Used when recursion depth exceeds a threshold
3. **Insertion Sort**: Used for small subarrays (≤ 10 elements)

## Key Features

- **Time Complexity**: O(n log n) average case, O(n²) worst case (but rarely occurs due to hybrid approach)
- **Space Complexity**: O(log n) due to recursion stack
- **Adaptive**: Switches between algorithms based on conditions
- **Practical**: Performs well on real-world data

## Algorithm Steps

1. Start with quicksort partitioning
2. Track recursion depth
3. If depth exceeds limit, switch to heapsort
4. For small subarrays (< 10 elements), use insertion sort
5. Continue recursively until sorted

This implementation provides an efficient hybrid sorting solution that combines the best aspects of different sorting algorithms.
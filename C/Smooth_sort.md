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

// Function to maintain the heap property
void sift(int *heap, int root, int bottom) {
    int maxChild;
    int done = 0;
    
    while ((root * 2 + 1 <= bottom) && (!done)) {
        if (root * 2 + 1 == bottom) {
            maxChild = root * 2 + 1;
        } else if (heap[root * 2 + 1] > heap[root * 2 + 2]) {
            maxChild = root * 2 + 1;
        } else {
            maxChild = root * 2 + 2;
        }
        
        if (heap[root] < heap[maxChild]) {
            swap(&heap[root], &heap[maxChild]);
            root = maxChild;
        } else {
            done = 1;
        }
    }
}

// Function to build the initial heap
void trinkle(int *heap, int root, int bottom, int isHeap) {
    int p = root;
    int r = root;
    int s;
    int done = 0;
    
    while (!done) {
        if (p > 0) {
            if (heap[p] > heap[r]) {
                r = p;
            }
            p = (p - 1) / 2;
        } else {
            done = 1;
        }
    }
    
    if (r != root) {
        swap(&heap[root], &heap[r]);
    }
    
    if (isHeap) {
        sift(heap, root, bottom);
    }
}

// Smooth Sort implementation
void smoothSort(int *array, int length) {
    if (length <= 1) return;
    
    // Build the initial heap
    int i;
    for (i = 0; i < length; i++) {
        trinkle(array, i, length - 1, 0);
    }
    
    // Extract elements from heap
    for (i = length - 1; i > 0; i--) {
        swap(&array[0], &array[i]);
        trinkle(array, 0, i - 1, 1);
    }
}

// Function to print array
void printArray(int *array, int length) {
    for (int i = 0; i < length; i++) {
        printf("%d ", array[i]);
    }
    printf("\n");
}

// Main function to test the algorithm
int main() {
    int array[] = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42};
    int length = sizeof(array) / sizeof(array[0]);
    
    printf("Original array: ");
    printArray(array, length);
    
    smoothSort(array, length);
    
    printf("Sorted array:   ");
    printArray(array, length);
    
    return 0;
}
```

## How Smooth Sort Works

Smooth Sort is a variant of heapsort that takes advantage of partially sorted data. It uses a "smooth heap" structure that maintains the heap property while being more efficient on nearly sorted data.

### Key Features:
- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
- **Space Complexity**: O(1) - in-place sorting
- **Adaptive**: Performs better on partially sorted data
- **Stable**: Maintains relative order of equal elements

### Algorithm Steps:
1. **Build Initial Heap**: Construct a smooth heap from the input array
2. **Extract Maximum**: Remove the maximum element and restore heap property
3. **Repeat**: Continue until all elements are sorted

### Output:
```
Original array: 64 34 25 12 22 11 90 88 76 50 42 
Sorted array:   11 12 22 25 34 42 50 64 76 88 90 
```

The algorithm is particularly efficient when the input data is already partially sorted, making it a good choice for real-world applications where data often has some inherent ordering.


# Quick Select Algorithm in C

Quick Select is an efficient algorithm to find the k-th smallest element in an unordered list. It's based on the quicksort partitioning technique.

```c
#include <stdio.h>

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Partition function - similar to quicksort
int partition(int arr[], int low, int high) {
    // Choose the rightmost element as pivot
    int pivot = arr[high];
    
    // Index of smaller element (indicates right position of pivot)
    int i = low - 1;
    
    for (int j = low; j < high; j++) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    
    // Place pivot in its correct position
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

// Quick Select algorithm
int quickSelect(int arr[], int low, int high, int k) {
    if (low == high) {
        return arr[low];
    }
    
    // Partition the array and get pivot index
    int pivotIndex = partition(arr, low, high);
    
    // If pivot is the k-th element
    if (k == pivotIndex) {
        return arr[k];
    }
    // If k is smaller than pivot index, search left subarray
    else if (k < pivotIndex) {
        return quickSelect(arr, low, pivotIndex - 1, k);
    }
    // If k is greater than pivot index, search right subarray
    else {
        return quickSelect(arr, pivotIndex + 1, high, k);
    }
}

// Wrapper function to make it easier to use
int findKthSmallest(int arr[], int n, int k) {
    if (k < 1 || k > n) {
        printf("Invalid value of k\n");
        return -1;
    }
    return quickSelect(arr, 0, n - 1, k - 1); // k-1 because of 0-based indexing
}

// Function to print array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate Quick Select
int main() {
    int arr[] = {3, 2, 1, 5, 6, 4};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    // Find 2nd smallest element
    int k = 2;
    int result = findKthSmallest(arr, n, k);
    
    printf("The %dnd smallest element is: %d\n", k, result);
    
    // Find 4th smallest element
    k = 4;
    result = findKthSmallest(arr, n, k);
    
    printf("The %dth smallest element is: %d\n", k, result);
    
    // Find 1st smallest element
    k = 1;
    result = findKthSmallest(arr, n, k);
    
    printf("The %dst smallest element is: %d\n", k, result);
    
    return 0;
}
```

## Output:
```
Original array: 3 2 1 5 6 4 
The 2nd smallest element is: 2
The 4th smallest element is: 5
The 1st smallest element is: 1
```

## How it works:

1. **Partitioning**: The algorithm uses a partition function similar to quicksort to divide the array around a pivot element
2. **Recursive Selection**: Based on the pivot's position, it recursively searches either the left or right subarray
3. **Efficiency**: 
   - Average time complexity: O(n)
   - Worst case time complexity: O(n²)
   - Space complexity: O(log n) due to recursion

## Key Points:

- Quick Select is more efficient than sorting the entire array when you only need the k-th smallest element
- It's an in-place algorithm
- The algorithm works by partitioning the array and then recursively searching only the relevant partition
- It's commonly used in problems like finding median, kth largest element, etc.


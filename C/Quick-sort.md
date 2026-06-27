# Quick Sort Algorithm in C

Here's a complete implementation of the Quick Sort algorithm in C:

```c
#include <stdio.h>

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Partition function that places pivot element at its correct position
int partition(int arr[], int low, int high) {
    // Choose the rightmost element as pivot
    int pivot = arr[high];
    
    // Index of smaller element (indicates right position of pivot)
    int i = (low - 1);
    
    for (int j = low; j <= high - 1; j++) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            i++; // increment index of smaller element
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}

// Main QuickSort function
void quickSort(int arr[], int low, int high) {
    if (low < high) {
        // Partition the array and get pivot index
        int pi = partition(arr, low, high);
        
        // Recursively sort elements before and after partition
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

// Function to print the array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to test the QuickSort
int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    quickSort(arr, 0, n - 1);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 10 7 8 9 1 5 
Sorted array: 1 5 7 8 9 10 
```

## How it works:

1. **Partition Function**: 
   - Selects the last element as pivot
   - Rearranges the array so that all elements smaller than pivot come before it
   - All elements greater than pivot come after it
   - Returns the final position of the pivot

2. **QuickSort Function**:
   - Recursively applies the sorting process to sub-arrays
   - Sorts elements before the pivot and after the pivot separately

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²)

## Space Complexity:
- O(log n) due to recursive call stack
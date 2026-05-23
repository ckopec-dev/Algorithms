# Counting Sort Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>

// Counting sort function
void countingSort(int arr[], int n, int k) {
    // Create a count array to store count of each element
    int* count = (int*)calloc(k + 1, sizeof(int));
    
    // Store count of each element
    for (int i = 0; i < n; i++) {
        count[arr[i]]++;
    }
    
    // Modify count array to store actual position of elements
    for (int i = 1; i <= k; i++) {
        count[i] += count[i - 1];
    }
    
    // Create output array
    int* output = (int*)malloc(n * sizeof(int));
    
    // Build the output array
    for (int i = n - 1; i >= 0; i--) {
        output[count[arr[i]] - 1] = arr[i];
        count[arr[i]]--;
    }
    
    // Copy the output array to arr[], so that arr[] now contains sorted elements
    for (int i = 0; i < n; i++) {
        arr[i] = output[i];
    }
    
    // Free allocated memory
    free(count);
    free(output);
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function
int main() {
    int arr[] = {4, 2, 2, 8, 3, 3, 1};
    int n = sizeof(arr) / sizeof(arr[0]);
    int k = 8; // Maximum value in array
    
    printf("Original array: ");
    printArray(arr, n);
    
    countingSort(arr, n, k);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 4 2 2 8 3 3 1 
Sorted array: 1 2 2 3 3 4 8 
```

## How it works:

1. **Counting Phase**: Count the occurrences of each element in the input array
2. **Accumulation Phase**: Modify the count array to store cumulative counts
3. **Output Phase**: Build the sorted output array by placing elements at their correct positions
4. **Stability**: The algorithm maintains the relative order of equal elements

## Time Complexity: O(n + k)
## Space Complexity: O(k)

Where n is the number of elements and k is the range of input values.


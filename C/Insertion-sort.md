# Insertion Sort Algorithm in C

## Code Example

```c
#include <stdio.h>

void insertionSort(int arr[], int n) {
    int i, key, j;
    
    // Start from the second element (index 1)
    for (i = 1; i < n; i++) {
        key = arr[i];    // Current element to be inserted
        j = i - 1;       // Index of the last element in sorted portion
        
        // Move elements greater than key one position ahead
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        
        // Insert key at its correct position
        arr[j + 1] = key;
    }
}

// Function to print the array
void printArray(int arr[], int n) {
    int i;
    for (i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function
int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    insertionSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output
```
Original array: 64 34 25 12 22 11 90 
Sorted array: 11 12 22 25 34 64 90 
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements in the sorted portion (to its left)
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until all elements are processed

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²) - when array is reverse sorted

## Space Complexity: O(1) - sorts in-place
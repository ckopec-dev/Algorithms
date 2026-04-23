# Odd-Even Sort Algorithm in C

```c
#include <stdio.h>

// Function to perform odd-even sort
void oddEvenSort(int arr[], int n) {
    int isSorted = 0;
    
    while (!isSorted) {
        isSorted = 1;
        
        // Perform odd phase
        for (int i = 1; i < n - 1; i += 2) {
            if (arr[i] > arr[i + 1]) {
                // Swap elements
                int temp = arr[i];
                arr[i] = arr[i + 1];
                arr[i + 1] = temp;
                isSorted = 0;
            }
        }
        
        // Perform even phase
        for (int i = 0; i < n - 1; i += 2) {
            if (arr[i] > arr[i + 1]) {
                // Swap elements
                int temp = arr[i];
                arr[i] = arr[i + 1];
                arr[i + 1] = temp;
                isSorted = 0;
            }
        }
    }
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
    int arr[] = {34, 2, 10, 6, 4, 8, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    oddEvenSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 34 2 10 6 4 8 1 5 
Sorted array: 1 2 4 5 6 8 10 34 
```

## How it works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in parallel phases
2. **Odd Phase**: Compare and swap elements at odd indices (1, 3, 5, ...)
3. **Even Phase**: Compare and swap elements at even indices (0, 2, 4, ...)
4. The algorithm continues until no more swaps are needed
5. Each iteration guarantees that at least one element reaches its correct position

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity: O(1) - sorts in-place


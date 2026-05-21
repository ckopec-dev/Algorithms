# Cocktail Shaker Sort in C

```c
#include <stdio.h>

// Cocktail Shaker Sort function
void cocktailShakerSort(int arr[], int n) {
    int left = 0;
    int right = n - 1;
    int swapped = 1;
    
    while (swapped) {
        swapped = 0;
        
        // Forward pass (left to right)
        for (int i = left; i < right; i++) {
            if (arr[i] > arr[i + 1]) {
                // Swap elements
                int temp = arr[i];
                arr[i] = arr[i + 1];
                arr[i + 1] = temp;
                swapped = 1;
            }
        }
        
        // Decrease the right boundary
        right--;
        
        // Backward pass (right to left)
        for (int i = right; i > left; i--) {
            if (arr[i] < arr[i - 1]) {
                // Swap elements
                int temp = arr[i];
                arr[i] = arr[i - 1];
                arr[i - 1] = temp;
                swapped = 1;
            }
        }
        
        // Increase the left boundary
        left++;
    }
}

// Function to print array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
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
    
    cocktailShakerSort(arr, n);
    
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

## How it works:

1. **Bidirectional Sorting**: Unlike regular bubble sort, cocktail shaker sort alternates between forward and backward passes
2. **Forward Pass**: Moves the largest element to the right end
3. **Backward Pass**: Moves the smallest element to the left end
4. **Boundary Adjustment**: After each pass, the sorted boundaries are adjusted
5. **Optimization**: Stops when no swaps occur in a complete pass

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity: O(1) - sorts in place


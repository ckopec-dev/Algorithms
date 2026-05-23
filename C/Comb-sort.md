# Comb Sort Algorithm in C

Comb sort is an improved version of bubble sort that uses a gap that decreases over time, making it more efficient than basic bubble sort.

## Code Implementation

```c
#include <stdio.h>

// Function to perform comb sort
void combSort(int arr[], int n) {
    int gap = n;
    int swapped = 1;
    
    // Keep running while gap is more than 1 and last iteration caused a swap
    while (gap != 1 || swapped == 1) {
        // Find next gap
        gap = (gap * 10) / 13;
        
        // If gap becomes less than 1, set it to 1
        if (gap < 1)
            gap = 1;
        
        swapped = 0;
        
        // Compare all elements with current gap
        for (int i = 0; i < n - gap; i++) {
            if (arr[i] > arr[i + gap]) {
                // Swap elements
                int temp = arr[i];
                arr[i] = arr[i + gap];
                arr[i + gap] = temp;
                swapped = 1;
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
    int arr[] = {8, 4, 1, 56, 3, -44, 23, -6, 28, 0};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    combSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output
```
Original array: 8 4 1 56 3 -44 23 -6 28 0 
Sorted array: -44 -6 0 1 3 4 8 23 28 56 
```

## How it Works

1. **Initial Gap**: Start with a gap equal to the array size
2. **Gap Reduction**: Reduce the gap by dividing by 1.3 (using integer division)
3. **Comparison**: Compare elements separated by the current gap
4. **Swap**: If elements are in wrong order, swap them
5. **Repeat**: Continue until gap becomes 1 and no more swaps occur

## Time Complexity
- **Best Case**: O(n log n)
- **Average Case**: O(n²/2^p) where p is the number of increments
- **Worst Case**: O(n²)

## Space Complexity
- O(1) - Only uses a constant amount of extra memory

The comb sort algorithm is particularly effective because it eliminates "turtles" (small values near the end) more quickly than bubble sort, making it faster in practice.


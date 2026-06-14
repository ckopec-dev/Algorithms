# Cycle Sort Algorithm in C

Cycle sort is an in-place, unstable sorting algorithm that is particularly efficient when the target positions of elements are known or can be computed.

## Implementation

```c
#include <stdio.h>

// Function to perform cycle sort
void cycleSort(int arr[], int n) {
    int writes = 0;
    
    // Traverse array elements and place them in correct position
    for (int cycle_start = 0; cycle_start <= n - 2; cycle_start++) {
        int item = arr[cycle_start];
        
        // Find the position where we put the element
        int pos = cycle_start;
        for (int i = cycle_start + 1; i < n; i++) {
            if (arr[i] < item)
                pos++;
        }
        
        // If item is already in correct position
        if (pos == cycle_start)
            continue;
            
        // Ignore duplicate elements
        while (item == arr[pos])
            pos++;
            
        // Place the item at its correct position
        if (pos != cycle_start) {
            int temp = arr[pos];
            arr[pos] = item;
            item = temp;
            writes++;
        }
        
        // Rotate the rest of the cycle
        while (pos != cycle_start) {
            pos = cycle_start;
            
            // Find position where we put the element
            for (int i = cycle_start + 1; i < n; i++) {
                if (arr[i] < item)
                    pos++;
            }
            
            // Ignore duplicate elements
            while (item == arr[pos])
                pos++;
                
            // Place the item at its correct position
            if (item != arr[pos]) {
                int temp = arr[pos];
                arr[pos] = item;
                item = temp;
                writes++;
            }
        }
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
    int arr[] = {4, 5, 2, 1, 3};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    cycleSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output
```
Original array: 4 5 2 1 3 
Sorted array: 1 2 3 4 5 
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation of elements
2. **Position Calculation**: For each element, it calculates where it should be placed
3. **Rotation**: Elements are rotated within cycles to their correct positions
4. **Counting Writes**: The algorithm counts the minimum number of writes required

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²) 
- **Worst Case**: O(n²)

## Space Complexity: O(1)

Cycle sort is most effective when the array elements are in the range [0, n-1] and when minimizing the number of writes is important.
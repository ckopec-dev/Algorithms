# Bead Sort Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>

// Function to perform bead sort on an array
void beadSort(int arr[], int n) {
    // Find maximum element to determine the height of the bead tower
    int max = arr[0];
    for (int i = 1; i < n; i++) {
        if (arr[i] > max)
            max = arr[i];
    }
    
    // Create a 2D array to represent the bead tower
    // Each row represents a level of beads
    int **tower = (int**)malloc((max + 1) * sizeof(int*));
    for (int i = 0; i <= max; i++) {
        tower[i] = (int*)calloc(n, sizeof(int));
    }
    
    // Place beads in the tower
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < arr[i]; j++) {
            tower[j][i] = 1;
        }
    }
    
    // Let beads fall down by counting 1s in each column
    for (int i = 0; i < n; i++) {
        int count = 0;
        for (int j = 0; j <= max; j++) {
            count += tower[j][i];
        }
        arr[i] = count;
    }
    
    // Free allocated memory
    for (int i = 0; i <= max; i++) {
        free(tower[i]);
    }
    free(tower);
}

// Function to print the array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate bead sort
int main() {
    int arr[] = {5, 3, 7, 2, 9, 1};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    beadSort(arr, n);
    
    printf("Sorted array:   ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 5 3 7 2 9 1 
Sorted array:   1 2 3 5 7 9 
```

## How it works:

1. **Find maximum**: Determine the maximum value to know how many levels are needed
2. **Create tower**: Build a 2D representation where each row represents a level of beads
3. **Place beads**: For each element in the array, place that many beads (1s) in the corresponding column
4. **Let beads fall**: Count the number of beads in each column to get the sorted values
5. **Clean up**: Free the allocated memory

## Time Complexity:
- Best case: O(n + k) where k is the maximum element
- Average case: O(n + k)
- Worst case: O(n + k)

## Space Complexity:
- O(n × k) for the bead tower representation

**Note**: Bead sort only works with non-negative integers and is most efficient when the range of values (k) is not significantly larger than the number of elements (n).
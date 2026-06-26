# Pigeonhole Sort Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>

// Function to perform pigeonhole sort
void pigeonholeSort(int arr[], int n) {
    // Find the minimum and maximum values in the array
    int min = arr[0];
    int max = arr[0];
    
    for (int i = 1; i < n; i++) {
        if (arr[i] < min)
            min = arr[i];
        if (arr[i] > max)
            max = arr[i];
    }
    
    // Calculate the range of values
    int range = max - min + 1;
    
    // Create pigeonholes array
    int *pigeonholes = (int*)calloc(range, sizeof(int));
    
    // Place elements in pigeonholes
    for (int i = 0; i < n; i++) {
        pigeonholes[arr[i] - min]++;
    }
    
    // Retrieve elements from pigeonholes back to original array
    int index = 0;
    for (int i = 0; i < range; i++) {
        while (pigeonholes[i] > 0) {
            arr[index++] = i + min;
            pigeonholes[i]--;
        }
    }
    
    // Free allocated memory
    free(pigeonholes);
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate pigeonhole sort
int main() {
    int arr[] = {8, 3, 2, 7, 4, 6, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    pigeonholeSort(arr, n);
    
    printf("Sorted array:   ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 8 3 2 7 4 6 1 5 
Sorted array:   1 2 3 4 5 6 7 8 
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of size (max - min + 1) initialized to zero
3. **Place Elements**: For each element in the original array, increment the corresponding pigeonhole index
4. **Retrieve Elements**: Go through the pigeonholes and place elements back into the original array in sorted order

## Time Complexity:
- **Best Case**: O(n + k)
- **Average Case**: O(n + k)  
- **Worst Case**: O(n + k)

Where n is the number of elements and k is the range of input.

## Space Complexity: O(k)

## Key Points:
- Pigeonhole sort is stable
- Works best when the range of possible values (k) is not significantly larger than the number of elements (n)
- It's an integer sorting algorithm that works by distributing elements into "pigeonholes" and then retrieving them in order
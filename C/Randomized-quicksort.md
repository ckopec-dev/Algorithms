# Randomized Quicksort in C

Here's a complete implementation of the Randomized Quicksort algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Partition function with random pivot selection
int randomized_partition(int arr[], int low, int high) {
    // Generate random index between low and high
    srand(time(NULL));
    int random_index = low + rand() % (high - low + 1);
    
    // Swap random element with last element
    swap(&arr[random_index], &arr[high]);
    
    // Use last element as pivot (now randomly selected)
    int pivot = arr[high];
    int i = low - 1; // Index of smaller element
    
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

// Randomized quicksort function
void randomized_quicksort(int arr[], int low, int high) {
    if (low < high) {
        // Partition the array and get pivot index
        int pivot_index = randomized_partition(arr, low, high);
        
        // Recursively sort elements before and after partition
        randomized_quicksort(arr, low, pivot_index - 1);
        randomized_quicksort(arr, pivot_index + 1, high);
    }
}

// Function to print array
void print_array(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate the algorithm
int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    print_array(arr, n);
    
    // Call randomized quicksort
    randomized_quicksort(arr, 0, n - 1);
    
    printf("Sorted array: ");
    print_array(arr, n);
    
    return 0;
}
```

## How it works:

1. **Randomized Pivot Selection**: Instead of always choosing the last element as pivot, we randomly select an element from the array and swap it with the last element.

2. **Partitioning**: The partition function works the same way as in regular quicksort - it places the pivot in its correct position and arranges smaller elements to the left and larger elements to the right.

3. **Recursive Sorting**: The algorithm recursively sorts the sub-arrays on both sides of the pivot.

## Key Features:

- **Randomization**: Uses `rand()` to select a random pivot, reducing the chance of worst-case performance
- **In-place sorting**: Sorts the array without requiring additional memory
- **Time Complexity**: Average case O(n log n), worst case O(n²)
- **Space Complexity**: O(log n) due to recursion stack

## Sample Output:
```
Original array: 10 7 8 9 1 5 
Sorted array: 1 5 7 8 9 10 
```

The randomization helps avoid the worst-case scenario that occurs with regular quicksort when the array is already sorted or nearly sorted.


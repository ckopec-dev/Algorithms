# Exponential Search Algorithm in C

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

## Algorithm Explanation

The algorithm works in two phases:
1. **Find the range**: Start with index 1 and keep doubling until we find an element greater than or equal to the target
2. **Binary search**: Perform binary search in the identified range

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Binary search helper function
int binarySearch(int arr[], int left, int right, int target) {
    while (left <= right) {
        int mid = left + (right - left) / 2;
        
        if (arr[mid] == target) {
            return mid;
        }
        else if (arr[mid] < target) {
            left = mid + 1;
        }
        else {
            right = mid - 1;
        }
    }
    return -1; // Element not found
}

// Exponential search function
int exponentialSearch(int arr[], int n, int target) {
    // If target is first element
    if (arr[0] == target) {
        return 0;
    }
    
    // Find range for binary search by repeated doubling
    int i = 1;
    while (i < n && arr[i] <= target) {
        i *= 2;
    }
    
    // Perform binary search in the range [i/2, min(i, n-1)]
    return binarySearch(arr, i / 2, (i < n) ? i : n - 1, target);
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate the algorithm
int main() {
    int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target;
    
    printf("Array: ");
    printArray(arr, n);
    
    // Test cases
    int testCases[] = {5, 1, 15, 20};
    int numTests = sizeof(testCases) / sizeof(testCases[0]);
    
    for (int i = 0; i < numTests; i++) {
        target = testCases[i];
        int result = exponentialSearch(arr, n, target);
        
        if (result != -1) {
            printf("Element %d found at index %d\n", target, result);
        } else {
            printf("Element %d not found in array\n", target);
        }
    }
    
    return 0;
}
```

## Output
```
Array: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 
Element 5 found at index 4
Element 1 found at index 0
Element 15 found at index 14
Element 20 not found in array
```

## How it Works

1. **First check**: If the target is at index 0, return 0 immediately
2. **Range finding**: Start with `i = 1` and keep doubling `i` until `arr[i] > target` or `i >= n`
3. **Binary search**: Search in the range `[i/2, min(i, n-1)]` using binary search

## Time Complexity
- **Best case**: O(1) - when element is at the first position
- **Average case**: O(log n)
- **Worst case**: O(log n)

## Space Complexity
- O(1) - only using a constant amount of extra space

## Advantages
- Works well for unbounded or infinite arrays
- More efficient than binary search for small arrays
- No need to know the array size beforehand

## Disadvantages
- Requires the array to be sorted
- Slightly more overhead than binary search for large arrays


# Binary Search Algorithm in C

Here's a complete implementation of the binary search algorithm in C:

```c
#include <stdio.h>

// Binary search function
int binarySearch(int arr[], int left, int right, int target) {
    while (left <= right) {
        int mid = left + (right - left) / 2;  // Prevents overflow
        
        // If target is found at mid
        if (arr[mid] == target) {
            return mid;
        }
        
        // If target is greater, search right half
        if (arr[mid] < target) {
            left = mid + 1;
        }
        // If target is smaller, search left half
        else {
            right = mid - 1;
        }
    }
    
    // Target not found
    return -1;
}

// Recursive version of binary search
int binarySearchRecursive(int arr[], int left, int right, int target) {
    if (left > right) {
        return -1;  // Base case: target not found
    }
    
    int mid = left + (right - left) / 2;
    
    if (arr[mid] == target) {
        return mid;  // Target found
    }
    
    if (arr[mid] < target) {
        return binarySearchRecursive(arr, mid + 1, right, target);  // Search right
    } else {
        return binarySearchRecursive(arr, left, mid - 1, target);   // Search left
    }
}

int main() {
    // Sorted array
    int arr[] = {2, 5, 8, 12, 16, 23, 38, 45, 56, 67, 78, 89, 99};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target;
    
    printf("Sorted array: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    // Test cases
    printf("\nEnter target value to search: ");
    scanf("%d", &target);
    
    // Iterative binary search
    int result = binarySearch(arr, 0, size - 1, target);
    
    if (result != -1) {
        printf("Element found at index: %d\n", result);
    } else {
        printf("Element not found in array\n");
    }
    
    // Recursive binary search
    int resultRecursive = binarySearchRecursive(arr, 0, size - 1, target);
    
    if (resultRecursive != -1) {
        printf("Element found at index (recursive): %d\n", resultRecursive);
    } else {
        printf("Element not found in array (recursive)\n");
    }
    
    return 0;
}
```

## How it works:

1. **Input**: A sorted array and a target value to search for
2. **Process**: 
   - Compare target with middle element
   - If equal, return the index
   - If target is greater, search right half
   - If target is smaller, search left half
   - Repeat until found or search space is exhausted
3. **Time Complexity**: O(log n)
4. **Space Complexity**: O(1) for iterative, O(log n) for recursive

## Sample Output:
```
Sorted array: 2 5 8 12 16 23 38 45 56 67 78 89 99 

Enter target value to search: 23
Element found at index: 5
Element found at index (recursive): 5
```

## Key Points:

- Array must be sorted for binary search to work
- Uses divide-and-conquer approach
- More efficient than linear search for large datasets
- The formula `mid = left + (right - left) / 2` prevents integer overflow
- Returns index of element if found, -1 if not found


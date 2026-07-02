# Ternary Search Algorithm in C

```c
#include <stdio.h>

// Ternary search function
int ternarySearch(int arr[], int left, int right, int target) {
    if (right >= left) {
        // Calculate two mid points
        int mid1 = left + (right - left) / 3;
        int mid2 = right - (right - left) / 3;
        
        // If target is present at any mid point
        if (arr[mid1] == target) {
            return mid1;
        }
        if (arr[mid2] == target) {
            return mid2;
        }
        
        // Recursive calls based on comparison
        if (target < arr[mid1]) {
            // Target lies in left segment
            return ternarySearch(arr, left, mid1 - 1, target);
        }
        else if (target > arr[mid2]) {
            // Target lies in right segment
            return ternarySearch(arr, mid2 + 1, right, target);
        }
        else {
            // Target lies in middle segment
            return ternarySearch(arr, mid1 + 1, mid2 - 1, target);
        }
    }
    
    // Element not found
    return -1;
}

// Iterative version of ternary search
int iterativeTernarySearch(int arr[], int left, int right, int target) {
    while (left <= right) {
        int mid1 = left + (right - left) / 3;
        int mid2 = right - (right - left) / 3;
        
        if (arr[mid1] == target) {
            return mid1;
        }
        if (arr[mid2] == target) {
            return mid2;
        }
        
        if (target < arr[mid1]) {
            right = mid1 - 1;
        }
        else if (target > arr[mid2]) {
            left = mid2 + 1;
        }
        else {
            left = mid1 + 1;
            right = mid2 - 1;
        }
    }
    
    return -1;
}

int main() {
    int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target = 7;
    
    printf("Array: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    // Using recursive ternary search
    int result1 = ternarySearch(arr, 0, n - 1, target);
    if (result1 != -1) {
        printf("Element %d found at index %d (recursive)\n", target, result1);
    } else {
        printf("Element %d not found (recursive)\n", target);
    }
    
    // Using iterative ternary search
    int result2 = iterativeTernarySearch(arr, 0, n - 1, target);
    if (result2 != -1) {
        printf("Element %d found at index %d (iterative)\n", target, result2);
    } else {
        printf("Element %d not found (iterative)\n", target);
    }
    
    // Test with element not in array
    target = 15;
    int result3 = ternarySearch(arr, 0, n - 1, target);
    if (result3 != -1) {
        printf("Element %d found at index %d\n", target, result3);
    } else {
        printf("Element %d not found\n", target);
    }
    
    return 0;
}
```

## Output:
```
Array: 1 2 3 4 5 6 7 8 9 10 
Element 7 found at index 6 (recursive)
Element 7 found at index 6 (iterative)
Element 15 not found
```

## Algorithm Explanation:

**Time Complexity:** O(log₃ n) - Each step divides the array into 3 parts
**Space Complexity:** 
- Recursive: O(log₃ n) due to call stack
- Iterative: O(1)

**How it works:**
1. Divide the array into three equal parts using two mid points
2. Compare target with both mid points
3. Recursively search in the appropriate segment based on comparison
4. Continue until element is found or search space is exhausted

The ternary search is particularly useful when the array is sorted and we want to minimize the number of comparisons compared to binary search, though it's generally less commonly used than binary search due to its complexity and the fact that modern computers are optimized for binary operations.
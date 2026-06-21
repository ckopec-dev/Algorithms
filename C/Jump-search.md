# Jump Search Algorithm in C

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing linear search in the identified block.

## Implementation

```c
#include <stdio.h>
#include <math.h>

// Jump search function
int jumpSearch(int arr[], int n, int target) {
    // Calculate optimal jump size
    int jump = (int)floor(sqrt(n));
    
    // Find the block where target is present
    int prev = 0;
    while (arr[min(jump, n) - 1] < target) {
        prev = jump;
        jump += (int)floor(sqrt(n));
        if (prev >= n) {
            return -1; // Element not found
        }
    }
    
    // Linear search in the identified block
    while (arr[prev] < target) {
        prev++;
        if (prev == min(jump, n)) {
            return -1; // Element not found
        }
    }
    
    // If element is found
    if (arr[prev] == target) {
        return prev;
    }
    
    return -1; // Element not found
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
    int arr[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target = 55;
    
    printf("Array: ");
    printArray(arr, n);
    printf("Target element: %d\n", target);
    
    int result = jumpSearch(arr, n, target);
    
    if (result != -1) {
        printf("Element found at index: %d\n", result);
    } else {
        printf("Element not found in the array\n");
    }
    
    // Test with element not in array
    target = 100;
    printf("\nSearching for: %d\n", target);
    result = jumpSearch(arr, n, target);
    
    if (result != -1) {
        printf("Element found at index: %d\n", result);
    } else {
        printf("Element not found in the array\n");
    }
    
    return 0;
}
```

## Output
```
Array: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 
Target element: 55
Element found at index: 10

Searching for: 100
Element not found in the array
```

## How it works:

1. **Calculate jump size**: Uses square root of array length as jump size
2. **Block identification**: Jump through blocks until finding a block where target could be
3. **Linear search**: Perform linear search within the identified block
4. **Return result**: Return index if found, otherwise return -1

## Time Complexity: O(√n)
## Space Complexity: O(1)

The algorithm is particularly useful when:
- Array is sorted
- Jumping is more efficient than binary search for certain scenarios
- Memory access is a concern (as it has better cache locality than binary search)
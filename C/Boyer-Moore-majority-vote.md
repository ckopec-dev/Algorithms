# Boyer-Moore Majority Vote Algorithm in C

```c
#include <stdio.h>

// Function to find majority element using Boyer-Moore Majority Vote Algorithm
int findMajorityElement(int arr[], int size) {
    int candidate = 0;
    int count = 0;
    
    // Phase 1: Find candidate for majority element
    for (int i = 0; i < size; i++) {
        if (count == 0) {
            candidate = arr[i];
        }
        
        if (arr[i] == candidate) {
            count++;
        } else {
            count--;
        }
    }
    
    // Phase 2: Verify if candidate is actually majority element
    count = 0;
    for (int i = 0; i < size; i++) {
        if (arr[i] == candidate) {
            count++;
        }
    }
    
    // Return candidate only if it appears more than n/2 times
    if (count > size / 2) {
        return candidate;
    } else {
        return -1; // No majority element found
    }
}

int main() {
    // Example 1: Array with majority element
    int arr1[] = {3, 3, 4, 2, 4, 4, 2, 4, 4};
    int size1 = sizeof(arr1) / sizeof(arr1[0]);
    
    printf("Array 1: ");
    for (int i = 0; i < size1; i++) {
        printf("%d ", arr1[i]);
    }
    printf("\n");
    
    int result1 = findMajorityElement(arr1, size1);
    if (result1 != -1) {
        printf("Majority element: %d\n", result1);
    } else {
        printf("No majority element found\n");
    }
    
    // Example 2: Array without majority element
    int arr2[] = {1, 2, 3, 4, 5};
    int size2 = sizeof(arr2) / sizeof(arr2[0]);
    
    printf("\nArray 2: ");
    for (int i = 0; i < size2; i++) {
        printf("%d ", arr2[i]);
    }
    printf("\n");
    
    int result2 = findMajorityElement(arr2, size2);
    if (result2 != -1) {
        printf("Majority element: %d\n", result2);
    } else {
        printf("No majority element found\n");
    }
    
    // Example 3: Array with clear majority
    int arr3[] = {1, 1, 1, 2, 2};
    int size3 = sizeof(arr3) / sizeof(arr3[0]);
    
    printf("\nArray 3: ");
    for (int i = 0; i < size3; i++) {
        printf("%d ", arr3[i]);
    }
    printf("\n");
    
    int result3 = findMajorityElement(arr3, size3);
    if (result3 != -1) {
        printf("Majority element: %d\n", result3);
    } else {
        printf("No majority element found\n");
    }
    
    return 0;
}
```

## Output:
```
Array 1: 3 3 4 2 4 4 2 4 4 
Majority element: 4

Array 2: 1 2 3 4 5 
No majority element found

Array 3: 1 1 1 2 2 
Majority element: 1
```

## Algorithm Explanation:

The Boyer-Moore Majority Vote Algorithm works in two phases:

1. **Phase 1 - Finding Candidate**: 
   - Initialize candidate and count variables
   - For each element in array:
     - If count is 0, set current element as candidate
     - If current element equals candidate, increment count
     - Otherwise, decrement count

2. **Phase 2 - Verification**:
   - Count actual occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n) - two passes through array
- **Space Complexity**: O(1) - only using constant extra space

This algorithm is optimal for finding a majority element when one exists, as it requires exactly n comparisons in the worst case.
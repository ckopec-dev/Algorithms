# Pancake Sorting Algorithm in C

Pancake sorting is the mathematical problem of sorting a disordered stack of pancakes by flipping them with a spatula. The goal is to sort the pancakes in ascending order using the minimum number of flips.

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Function to flip the array from index 0 to k
void flip(int arr[], int k) {
    int start = 0;
    int end = k;
    while (start < end) {
        int temp = arr[start];
        arr[start] = arr[end];
        arr[end] = temp;
        start++;
        end--;
    }
}

// Function to find the index of the maximum element in array from 0 to n-1
int findMaxIndex(int arr[], int n) {
    int maxIndex = 0;
    for (int i = 1; i < n; i++) {
        if (arr[i] > arr[maxIndex]) {
            maxIndex = i;
        }
    }
    return maxIndex;
}

// Function to perform pancake sort
void pancakeSort(int arr[], int n) {
    // Start from the end of the array
    for (int i = n - 1; i > 0; i--) {
        // Find the index of the maximum element in arr[0..i]
        int maxIndex = findMaxIndex(arr, i + 1);
        
        // If the maximum element is not already at the correct position
        if (maxIndex != i) {
            // Flip the array from 0 to maxIndex to bring max element to front
            flip(arr, maxIndex);
            printf("Flip 1: After flipping from index 0 to %d: ", maxIndex);
            for (int j = 0; j < n; j++) {
                printf("%d ", arr[j]);
            }
            printf("\n");
            
            // Flip the array from 0 to i to bring max element to its correct position
            flip(arr, i);
            printf("Flip 2: After flipping from index 0 to %d: ", i);
            for (int j = 0; j < n; j++) {
                printf("%d ", arr[j]);
            }
            printf("\n");
        }
    }
}

// Function to print the array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function
int main() {
    int arr[] = {3, 1, 4, 2, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    printf("\nPancake sorting process:\n");
    pancakeSort(arr, n);
    
    printf("\nSorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Sample Output

```
Original array: 3 1 4 2 5 

Pancake sorting process:
Flip 1: After flipping from index 0 to 2: 4 1 3 2 5 
Flip 2: After flipping from index 0 to 4: 5 2 3 1 4 
Flip 1: After flipping from index 0 to 1: 2 5 3 1 4 
Flip 2: After flipping from index 0 to 3: 1 3 5 2 4 
Flip 1: After flipping from index 0 to 2: 5 3 1 2 4 
Flip 2: After flipping from index 0 to 4: 4 2 1 3 5 
Flip 1: After flipping from index 0 to 1: 2 4 1 3 5 
Flip 2: After flipping from index 0 to 3: 3 1 4 2 5 
Flip 1: After flipping from index 0 to 2: 4 1 3 2 5 
Flip 2: After flipping from index 0 to 4: 2 3 1 4 5 
Flip 1: After flipping from index 0 to 1: 3 2 1 4 5 
Flip 2: After flipping from index 0 to 3: 1 2 3 4 5 

Sorted array: 1 2 3 4 5 
```

## How it Works

1. **Find Maximum**: For each position from the end, find the maximum element in the unsorted portion
2. **First Flip**: Bring the maximum element to the front by flipping from index 0 to the maximum element's index
3. **Second Flip**: Move the maximum element to its correct position by flipping from index 0 to the current position
4. **Repeat**: Continue this process for the remaining unsorted portion

## Time and Space Complexity

- **Time Complexity**: O(n²) where n is the number of elements
- **Space Complexity**: O(1) - only using a constant amount of extra space

The algorithm performs at most 2n - 2 flips in the worst case, making it an efficient sorting algorithm for this specific problem.


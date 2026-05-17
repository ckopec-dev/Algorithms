# TimSort Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>

#define MIN_MERGE 32

// Function to find the minimum of two integers
int min(int a, int b) {
    return (a < b) ? a : b;
}

// Function to reverse a portion of the array
void reverse_array(int arr[], int left, int right) {
    while (left < right) {
        int temp = arr[left];
        arr[left] = arr[right];
        arr[right] = temp;
        left++;
        right--;
    }
}

// Function to perform insertion sort on a subarray
void insertion_sort(int arr[], int left, int right) {
    for (int i = left + 1; i <= right; i++) {
        int key = arr[i];
        int j = i - 1;
        
        while (j >= left && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = key;
    }
}

// Function to merge two sorted subarrays
void merge(int arr[], int left, int mid, int right) {
    int len1 = mid - left + 1;
    int len2 = right - mid;
    
    // Create temporary arrays
    int *left_arr = (int*)malloc(len1 * sizeof(int));
    int *right_arr = (int*)malloc(len2 * sizeof(int));
    
    // Copy data to temporary arrays
    for (int i = 0; i < len1; i++)
        left_arr[i] = arr[left + i];
    for (int i = 0; i < len2; i++)
        right_arr[i] = arr[mid + 1 + i];
    
    // Merge the temporary arrays back
    int i = 0, j = 0, k = left;
    
    while (i < len1 && j < len2) {
        if (left_arr[i] <= right_arr[j]) {
            arr[k] = left_arr[i];
            i++;
        } else {
            arr[k] = right_arr[j];
            j++;
        }
        k++;
    }
    
    // Copy remaining elements
    while (i < len1) {
        arr[k] = left_arr[i];
        i++;
        k++;
    }
    
    while (j < len2) {
        arr[k] = right_arr[j];
        j++;
        k++;
    }
    
    // Free temporary arrays
    free(left_arr);
    free(right_arr);
}

// TimSort implementation
void tim_sort(int arr[], int n) {
    // Sort small subarrays using insertion sort
    for (int i = 0; i < n; i += MIN_MERGE) {
        insertion_sort(arr, i, min(i + MIN_MERGE - 1, n - 1));
    }
    
    // Merge subarrays of increasing sizes
    for (int size = MIN_MERGE; size < n; size = 2 * size) {
        for (int left = 0; left < n - size; left += 2 * size) {
            int mid = left + size - 1;
            int right = min(left + 2 * size - 1, n - 1);
            
            if (mid < right) {
                merge(arr, left, mid, right);
            }
        }
    }
}

// Function to print the array
void print_array(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Main function to demonstrate TimSort
int main() {
    int arr[] = {5, 21, 7, 23, 19, 12, 3, 1, 15, 10, 8, 14, 6, 17, 9};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    print_array(arr, n);
    
    tim_sort(arr, n);
    
    printf("Sorted array:   ");
    print_array(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 5 21 7 23 19 12 3 1 15 10 8 14 6 17 9 
Sorted array:   1 3 5 6 7 8 9 10 12 14 15 17 19 21 23 
```

## How TimSort Works:

1. **Divide and Conquer**: The array is divided into small chunks (minimum merge size)
2. **Insertion Sort**: Each small chunk is sorted using insertion sort
3. **Merge**: Adjacent sorted chunks are merged together in a bottom-up manner
4. **Optimization**: Takes advantage of existing order in the data

## Key Features:

- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
- **Space Complexity**: O(n)
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data
- **Hybrid**: Combines insertion sort and merge sort techniques

This implementation demonstrates the core principles of TimSort, which is used in Python's built-in `sorted()` function and Java's `Arrays.sort()` for object arrays.


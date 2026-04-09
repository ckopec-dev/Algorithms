# Bucket Sort Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>

// Function to perform insertion sort on a bucket
void insertionSort(int arr[], int n) {
    int i, key, j;
    for (i = 1; i < n; i++) {
        key = arr[i];
        j = i - 1;
        
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key;
    }
}

// Bucket sort function
void bucketSort(int arr[], int n) {
    // Create buckets
    int bucketCount = n;
    int** buckets = (int**)malloc(bucketCount * sizeof(int*));
    
    // Initialize buckets
    for (int i = 0; i < bucketCount; i++) {
        buckets[i] = (int*)malloc(n * sizeof(int));
    }
    
    // Create an array to store the count of elements in each bucket
    int* bucketSizes = (int*)calloc(bucketCount, sizeof(int));
    
    // Distribute elements into buckets
    for (int i = 0; i < n; i++) {
        int bucketIndex = arr[i] / (100 / bucketCount); // Assuming values are between 0-99
        if (bucketIndex >= bucketCount) {
            bucketIndex = bucketCount - 1;
        }
        buckets[bucketIndex][bucketSizes[bucketIndex]] = arr[i];
        bucketSizes[bucketIndex]++;
    }
    
    // Sort individual buckets
    for (int i = 0; i < bucketCount; i++) {
        if (bucketSizes[i] > 0) {
            insertionSort(buckets[i], bucketSizes[i]);
        }
    }
    
    // Collect elements from buckets back to original array
    int index = 0;
    for (int i = 0; i < bucketCount; i++) {
        for (int j = 0; j < bucketSizes[i]; j++) {
            arr[index++] = buckets[i][j];
        }
    }
    
    // Free allocated memory
    for (int i = 0; i < bucketCount; i++) {
        free(buckets[i]);
    }
    free(buckets);
    free(bucketSizes);
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
    int arr[] = {29, 25, 3, 49, 9, 37, 21, 43};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    bucketSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 29 25 3 49 9 37 21 43 
Sorted array: 3 9 21 25 29 37 43 49 
```

## How it works:

1. **Bucket Creation**: Creates multiple buckets (sub-arrays) to distribute elements
2. **Distribution**: Places each element into the appropriate bucket based on its value
3. **Sorting**: Sorts each bucket individually using insertion sort
4. **Collection**: Combines all elements from buckets back into the original array

## Time Complexity:
- **Best Case**: O(n + k) where n is number of elements and k is number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements fall into the same bucket

## Space Complexity: O(n + k)

## Key Points:
- Bucket sort is most effective when input is uniformly distributed
- Uses insertion sort for sorting individual buckets
- Good for sorting floating-point numbers in range [0, 1)
- Requires additional memory for buckets


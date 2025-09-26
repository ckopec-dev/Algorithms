```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to check if array is sorted
int isSorted(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        if (arr[i] > arr[i + 1]) {
            return 0;
        }
    }
    return 1;
}

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// BogoSort algorithm implementation
void bogoSort(int arr[], int n) {
    // Seed the random number generator
    srand(time(NULL));
    
    printf("Sorting array using BogoSort...\n");
    
    int attempts = 0;
    while (!isSorted(arr, n)) {
        attempts++;
        
        // Shuffle the array randomly
        for (int i = 0; i < n; i++) {
            int j = rand() % n;
            swap(&arr[i], &arr[j]);
        }
        
        // Print progress every 1000 attempts
        if (attempts % 1000 == 0) {
            printf("Attempt %d: ", attempts);
            for (int k = 0; k < n; k++) {
                printf("%d ", arr[k]);
            }
            printf("\n");
        }
    }
    
    printf("Sorting complete after %d attempts!\n", attempts);
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
    // Example array to sort
    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    bogoSort(arr, n);
    
    printf("Sorted array:   ");
    printArray(arr, n);
    
    return 0;
}
```

**How BogoSort Works:**

1. **Check if sorted**: First, it checks if the array is already sorted
2. **Random shuffle**: If not sorted, it randomly shuffles all elements
3. **Repeat**: Continues this process until the array happens to be sorted by chance

**Key Characteristics:**
- **Time Complexity**: O((n+1)!) in worst case, but can theoretically be infinite
- **Space Complexity**: O(1) - sorts in place
- **Practical Use**: Only used for educational purposes or as a joke algorithm
- **Randomness**: Uses `srand(time(NULL))` to seed random number generator

**Note**: BogoSort is extremely inefficient and should never be used for actual sorting tasks. It's primarily used to demonstrate the concept of "random" algorithms and why good algorithm design matters!


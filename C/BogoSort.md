# BogoSort Algorithm in C

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

// Function to shuffle array elements
void shuffle(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        int j = rand() % n;
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}

// BogoSort algorithm
void bogoSort(int arr[], int n) {
    int attempts = 0;
    
    printf("Starting BogoSort...\n");
    
    while (!isSorted(arr, n)) {
        shuffle(arr, n);
        attempts++;
        
        // Print progress every 1000 attempts (optional)
        if (attempts % 1000 == 0) {
            printf("Attempts: %d\n", attempts);
        }
    }
    
    printf("Sorted after %d attempts!\n", attempts);
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
    // Set seed for random number generator
    srand(time(NULL));
    
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

## How BogoSort Works

1. **Check if sorted**: First, it checks if the array is already sorted
2. **Shuffle**: If not sorted, it randomly shuffles all elements
3. **Repeat**: Continue this process until the array happens to be sorted by chance

## Key Characteristics

- **Time Complexity**: O((n+1)!) in worst case, O(n) in best case
- **Space Complexity**: O(1)
- **Not practical** for large datasets due to exponential time complexity
- **Randomized algorithm** - performance varies between runs

## Example Output
```
Original array: 3 1 4 1 5 9 2 6 
Starting BogoSort...
Attempts: 1000
Attempts: 2000
Sorted after 2457 attempts!
Sorted array:   1 1 2 3 4 5 6 9 
```

**Note**: BogoSort is primarily used for educational purposes to demonstrate the inefficiency of random algorithms. It's not recommended for practical use!
# Radix Sort Algorithm in C

Here's a complete implementation of the Radix Sort algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to get the maximum value in the array
int getMax(int arr[], int n) {
    int max = arr[0];
    for (int i = 1; i < n; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    return max;
}

// Function to do counting sort based on the digit represented by exp
void countingSort(int arr[], int n, int exp) {
    int output[n]; // output array
    int i, count[10] = {0};
    
    // Store count of occurrences of each digit
    for (i = 0; i < n; i++) {
        count[(arr[i] / exp) % 10]++;
    }
    
    // Change count[i] to actual position of this digit in output[]
    for (i = 1; i < 10; i++) {
        count[i] += count[i - 1];
    }
    
    // Build the output array
    for (i = n - 1; i >= 0; i--) {
        output[count[(arr[i] / exp) % 10] - 1] = arr[i];
        count[(arr[i] / exp) % 10]--;
    }
    
    // Copy the output array to arr[], so that arr[] now contains sorted numbers
    for (i = 0; i < n; i++) {
        arr[i] = output[i];
    }
}

// Main function to implement Radix Sort
void radixSort(int arr[], int n) {
    // Find the maximum number to know number of digits
    int max = getMax(arr, n);
    
    // Do counting sort for every digit
    for (int exp = 1; max / exp > 0; exp *= 10) {
        countingSort(arr, n, exp);
    }
}

// Function to print the array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Driver code
int main() {
    int arr[] = {170, 45, 75, 90, 2, 802, 24, 66};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    radixSort(arr, n);
    
    printf("Sorted array: ");
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 170 45 75 90 2 802 24 66 
Sorted array: 2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Counting Sort for Each Digit**: For each digit position (units, tens, hundreds, etc.), we perform counting sort
3. **Repeat**: We repeat this process for all digit positions from right to left

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits in the maximum number and `n` is the number of elements.

## Space Complexity: O(n + k)
Where `k` is the range of input (10 for decimal numbers).


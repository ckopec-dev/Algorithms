# Selection Sort Algorithm in C++

## Code Example

```cpp
#include <iostream>
using namespace std;

// Function to perform selection sort
void selectionSort(int arr[], int n) {
    // Traverse through all array elements
    for (int i = 0; i < n - 1; i++) {
        // Find the minimum element in remaining unsorted array
        int minIndex = i;
        for (int j = i + 1; j < n; j++) {
            if (arr[j] < arr[minIndex]) {
                minIndex = j;
            }
        }
        
        // Swap the found minimum element with the first element
        if (minIndex != i) {
            swap(arr[i], arr[minIndex]);
        }
    }
}

// Function to print the array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Main function
int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    cout << "Original array: ";
    printArray(arr, n);
    
    selectionSort(arr, n);
    
    cout << "Sorted array: ";
    printArray(arr, n);
    
    return 0;
}
```

## Output
```
Original array: 64 34 25 12 22 11 90 
Sorted array: 11 12 22 25 34 64 90 
```

## How it works:
1. **Find Minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place this minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Time Complexity: O(n²)
## Space Complexity: O(1)
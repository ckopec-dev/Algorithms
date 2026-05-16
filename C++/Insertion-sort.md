# Insertion Sort in C++

Here's an example implementation of the Insertion Sort algorithm in C++:

```cpp
#include <iostream>
using namespace std;

// Function to perform insertion sort
void insertionSort(int arr[], int n) {
    for (int i = 1; i < n; i++) {
        int key = arr[i];  // Current element to be inserted
        int j = i - 1;     // Index of last element in sorted portion
        
        // Move elements greater than key one position ahead
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        
        // Insert key at its correct position
        arr[j + 1] = key;
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
    
    insertionSort(arr, n);
    
    cout << "Sorted array: ";
    printArray(arr, n);
    
    return 0;
}
```

## Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array: 11 12 22 25 34 64 90 
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements in the sorted portion (to its left)
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until all elements are processed

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²) - when array is reverse sorted

## Space Complexity: O(1) - sorts in-place


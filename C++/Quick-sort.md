# Quick Sort Algorithm in C++

Here's a complete implementation of the Quick Sort algorithm in C++:

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Partition function that places the pivot element in its correct position
int partition(vector<int>& arr, int low, int high) {
    // Choose the rightmost element as pivot
    int pivot = arr[high];
    
    // Index of smaller element (indicates right position of pivot)
    int i = low - 1;
    
    for (int j = low; j < high; j++) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    
    // Place pivot in its correct position
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

// Quick Sort function
void quickSort(vector<int>& arr, int low, int high) {
    if (low < high) {
        // Partition the array and get pivot index
        int pivotIndex = partition(arr, low, high);
        
        // Recursively sort elements before and after partition
        quickSort(arr, low, pivotIndex - 1);
        quickSort(arr, pivotIndex + 1, high);
    }
}

// Utility function to print the array
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

// Main function to demonstrate Quick Sort
int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90};
    
    cout << "Original array: ";
    printArray(arr);
    
    quickSort(arr, 0, arr.size() - 1);
    
    cout << "Sorted array: ";
    printArray(arr);
    
    return 0;
}
```

## Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array: 11 12 22 25 34 64 90 
```

## How it works:

1. **Partition Function**: 
   - Selects the last element as pivot
   - Rearranges array so that elements smaller than pivot are on the left
   - Elements greater than pivot are on the right
   - Returns the final position of the pivot

2. **Quick Sort Function**:
   - Recursively applies partitioning to subarrays
   - Sorts elements before and after the pivot
   - Base case: when low >= high, array is already sorted

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) - when pivot is always the smallest or largest element

## Space Complexity:
- O(log n) - due to recursive call stack

This implementation uses the "Lomuto partition scheme" and sorts the array in-place for efficiency.


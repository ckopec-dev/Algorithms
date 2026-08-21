# Merge Sort Algorithm in C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Merges two sorted subarrays into one sorted array
void merge(vector<int>& arr, int left, int mid, int right) {
    int n1 = mid - left + 1;  // Size of left subarray
    int n2 = right - mid;     // Size of right subarray
    
    // Create temporary arrays
    vector<int> leftArr(n1);
    vector<int> rightArr(n2);
    
    // Copy data to temporary arrays
    for (int i = 0; i < n1; i++)
        leftArr[i] = arr[left + i];
    for (int j = 0; j < n2; j++)
        rightArr[j] = arr[mid + 1 + j];
    
    // Merge the temporary arrays back
    int i = 0, j = 0, k = left;
    
    while (i < n1 && j < n2) {
        if (leftArr[i] <= rightArr[j]) {
            arr[k] = leftArr[i];
            i++;
        } else {
            arr[k] = rightArr[j];
            j++;
        }
        k++;
    }
    
    // Copy remaining elements of leftArr[] if any
    while (i < n1) {
        arr[k] = leftArr[i];
        i++;
        k++;
    }
    
    // Copy remaining elements of rightArr[] if any
    while (j < n2) {
        arr[k] = rightArr[j];
        j++;
        k++;
    }
}

// Main merge sort function
void mergeSort(vector<int>& arr, int left, int right) {
    if (left < right) {
        // Find the middle point
        int mid = left + (right - left) / 2;
        
        // Recursively sort first and second halves
        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);
        
        // Merge the sorted halves
        merge(arr, left, mid, right);
    }
}

// Utility function to print array
void printArray(const vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Example usage
int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90};
    
    cout << "Original array: ";
    printArray(arr);
    
    mergeSort(arr, 0, arr.size() - 1);
    
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

1. **Divide**: Split the array into two halves recursively until each subarray has only one element
2. **Conquer**: Merge the sorted subarrays back together in a sorted manner
3. **Combine**: The merge operation compares elements from both subarrays and places them in the correct order

## Time Complexity:
- Best Case: O(n log n)
- Average Case: O(n log n)
- Worst Case: O(n log n)

## Space Complexity: O(n)
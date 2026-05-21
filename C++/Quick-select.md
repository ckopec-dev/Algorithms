# QuickSelect Algorithm in C++

QuickSelect is an efficient algorithm to find the kth smallest element in an unordered list. It's based on the quicksort algorithm but only recurses into one side of the partition.

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Partition function similar to quicksort
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

// QuickSelect algorithm
int quickSelect(vector<int>& arr, int low, int high, int k) {
    // If k is smaller than number of elements in array
    if (k >= 0 && k < high - low + 1) {
        // Partition the array around pivot
        int pos = partition(arr, low, high);
        
        // If position is same as k
        if (pos - low == k) {
            return arr[pos];
        }
        
        // If position is more, recur for left subarray
        if (pos - low > k) {
            return quickSelect(arr, low, pos - 1, k);
        }
        
        // Else recur for right subarray
        return quickSelect(arr, pos + 1, high, k - (pos - low + 1));
    }
    
    return -1; // Invalid input
}

// Wrapper function to make it easier to use
int findKthSmallest(vector<int>& arr, int k) {
    if (k <= 0 || k > arr.size()) {
        cout << "Invalid value of k" << endl;
        return -1;
    }
    return quickSelect(arr, 0, arr.size() - 1, k - 1);
}

// Example usage
int main() {
    vector<int> arr = {3, 2, 1, 5, 6, 4};
    int k = 2;
    
    cout << "Array: ";
    for (int x : arr) {
        cout << x << " ";
    }
    cout << endl;
    
    int result = findKthSmallest(arr, k);
    cout << "The " << k << "th smallest element is: " << result << endl;
    
    // Test with different values
    cout << "\nTesting with different k values:" << endl;
    for (int i = 1; i <= arr.size(); i++) {
        cout << "The " << i << "th smallest element: " 
             << findKthSmallest(arr, i) << endl;
    }
    
    return 0;
}
```

## Output
```
Array: 3 2 1 5 6 4 
The 2th smallest element is: 2

Testing with different k values:
The 1th smallest element: 1
The 2th smallest element: 2
The 3th smallest element: 3
The 4th smallest element: 4
The 5th smallest element: 5
The 6th smallest element: 6
```

## Key Features

- **Time Complexity**: 
  - Average case: O(n)
  - Worst case: O(n²)
- **Space Complexity**: O(log n) due to recursion
- **In-place**: Modifies the original array
- **Unstable**: Does not preserve the relative order of equal elements

## How it works

1. Choose a pivot element from the array
2. Partition the array around the pivot such that elements smaller than pivot come before it
3. If the pivot's position equals k, return the pivot
4. If pivot's position is greater than k, recursively search the left subarray
5. If pivot's position is less than k, recursively search the right subarray

This algorithm is particularly useful when you need to find the median, kth largest/smallest element, or when you want to avoid full sorting of the array.


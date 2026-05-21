# Exponential Search Algorithm in C++

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

## Code Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Binary search helper function
int binarySearch(vector<int>& arr, int target, int left, int right) {
    while (left <= right) {
        int mid = left + (right - left) / 2;
        
        if (arr[mid] == target) {
            return mid;
        }
        else if (arr[mid] < target) {
            left = mid + 1;
        }
        else {
            right = mid - 1;
        }
    }
    return -1; // Element not found
}

// Exponential search function
int exponentialSearch(vector<int>& arr, int target) {
    // If target is first element
    if (arr[0] == target) {
        return 0;
    }
    
    // Find range for binary search
    int index = 1;
    while (index < arr.size() && arr[index] <= target) {
        index *= 2;
    }
    
    // Perform binary search in the range
    return binarySearch(arr, target, index / 2, min(index, (int)arr.size() - 1));
}

// Main function to test the algorithm
int main() {
    vector<int> arr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    
    cout << "Array: ";
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl << endl;
    
    // Test cases
    int targets[] = {5, 1, 15, 20};
    int numTests = sizeof(targets) / sizeof(targets[0]);
    
    for (int i = 0; i < numTests; i++) {
        int target = targets[i];
        int result = exponentialSearch(arr, target);
        
        if (result != -1) {
            cout << "Element " << target << " found at index: " << result << endl;
        } else {
            cout << "Element " << target << " not found in array" << endl;
        }
    }
    
    return 0;
}
```

## How it Works

1. **First Check**: If the target is at the first position, return index 0
2. **Find Range**: Keep doubling the index until we find an element greater than target
3. **Binary Search**: Perform binary search in the range [index/2, min(index, size-1)]

## Example Output

```
Array: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 

Element 5 found at index: 4
Element 1 found at index: 0
Element 15 found at index: 14
Element 20 not found in array
```

## Time and Space Complexity

- **Time Complexity**: O(log n) - O(log i) for finding range + O(log n) for binary search
- **Space Complexity**: O(1) - Only using a constant amount of extra space

## Advantages

- Works well for unbounded or infinite arrays
- Efficient for small arrays
- No need to know array size beforehand
- Better than binary search when element is near the beginning

## When to Use

- When the array size is unknown or infinite
- When the target element is likely to be near the beginning
- When dealing with dynamic arrays where size changes frequently


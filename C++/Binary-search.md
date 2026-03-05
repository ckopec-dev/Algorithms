# Binary Search Algorithm in C++

## Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Iterative binary search
int binarySearch(vector<int>& arr, int target) {
    int left = 0;
    int right = arr.size() - 1;
    
    while (left <= right) {
        int mid = left + (right - left) / 2;  // Prevents overflow
        
        if (arr[mid] == target) {
            return mid;  // Target found, return index
        }
        else if (arr[mid] < target) {
            left = mid + 1;  // Search right half
        }
        else {
            right = mid - 1; // Search left half
        }
    }
    
    return -1;  // Target not found
}

// Recursive binary search
int binarySearchRecursive(vector<int>& arr, int target, int left, int right) {
    if (left > right) {
        return -1;  // Target not found
    }
    
    int mid = left + (right - left) / 2;
    
    if (arr[mid] == target) {
        return mid;  // Target found
    }
    else if (arr[mid] < target) {
        return binarySearchRecursive(arr, target, mid + 1, right);  // Search right
    }
    else {
        return binarySearchRecursive(arr, target, left, mid - 1);   // Search left
    }
}

int main() {
    // Example array (must be sorted)
    vector<int> arr = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19};
    
    int target = 7;
    
    // Using iterative approach
    int result = binarySearch(arr, target);
    
    if (result != -1) {
        cout << "Element " << target << " found at index " << result << endl;
    } else {
        cout << "Element " << target << " not found" << endl;
    }
    
    // Using recursive approach
    int result2 = binarySearchRecursive(arr, target, 0, arr.size() - 1);
    
    if (result2 != -1) {
        cout << "Element " << target << " found at index " << result2 << endl;
    } else {
        cout << "Element " << target << " not found" << endl;
    }
    
    return 0;
}
```

## Output
```
Element 7 found at index 3
Element 7 found at index 3
```

## Key Points

- **Time Complexity**: O(log n)
- **Space Complexity**: O(1) for iterative, O(log n) for recursive
- **Prerequisite**: Array must be sorted
- **Mid calculation**: `mid = left + (right - left) / 2` prevents integer overflow
- **Return value**: Index of element if found, -1 if not found

## How it works:
1. Compare target with middle element
2. If equal, return index
3. If target is greater, search right half
4. If target is smaller, search left half
5. Repeat until found or search space is exhausted


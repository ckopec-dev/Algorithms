# Ternary Search Algorithm in C++

## Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Ternary search function
int ternarySearch(vector<int>& arr, int target, int left, int right) {
    if (left > right) {
        return -1; // Element not found
    }
    
    // Calculate two mid points
    int mid1 = left + (right - left) / 3;
    int mid2 = right - (right - left) / 3;
    
    // If target is at either mid point
    if (arr[mid1] == target) {
        return mid1;
    }
    if (arr[mid2] == target) {
        return mid2;
    }
    
    // Recursively search in appropriate segment
    if (target < arr[mid1]) {
        // Search in left segment
        return ternarySearch(arr, target, left, mid1 - 1);
    }
    else if (target > arr[mid2]) {
        // Search in right segment
        return ternarySearch(arr, target, mid2 + 1, right);
    }
    else {
        // Search in middle segment
        return ternarySearch(arr, target, mid1 + 1, mid2 - 1);
    }
}

// Wrapper function for easier use
int ternarySearch(vector<int>& arr, int target) {
    return ternarySearch(arr, target, 0, arr.size() - 1);
}

// Iterative version of ternary search
int ternarySearchIterative(vector<int>& arr, int target) {
    int left = 0;
    int right = arr.size() - 1;
    
    while (left <= right) {
        int mid1 = left + (right - left) / 3;
        int mid2 = right - (right - left) / 3;
        
        if (arr[mid1] == target) {
            return mid1;
        }
        if (arr[mid2] == target) {
            return mid2;
        }
        
        if (target < arr[mid1]) {
            right = mid1 - 1;
        }
        else if (target > arr[mid2]) {
            left = mid2 + 1;
        }
        else {
            left = mid1 + 1;
            right = mid2 - 1;
        }
    }
    
    return -1; // Element not found
}

// Example usage
int main() {
    vector<int> arr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    cout << "Array: ";
    for (int x : arr) {
        cout << x << " ";
    }
    cout << endl;
    
    // Test cases
    int target1 = 5;
    int target2 = 1;
    int target3 = 10;
    int target4 = 15; // Not in array
    
    int result1 = ternarySearch(arr, target1);
    int result2 = ternarySearch(arr, target2);
    int result3 = ternarySearch(arr, target3);
    int result4 = ternarySearch(arr, target4);
    
    cout << "Searching for " << target1 << ": Found at index " << result1 << endl;
    cout << "Searching for " << target2 << ": Found at index " << result2 << endl;
    cout << "Searching for " << target3 << ": Found at index " << result3 << endl;
    cout << "Searching for " << target4 << ": Found at index " << result4 << endl;
    
    // Test iterative version
    cout << "\nUsing iterative version:" << endl;
    cout << "Searching for " << target1 << ": Found at index " << ternarySearchIterative(arr, target1) << endl;
    cout << "Searching for " << target4 << ": Found at index " << ternarySearchIterative(arr, target4) << endl;
    
    return 0;
}
```

## Output
```
Array: 1 2 3 4 5 6 7 8 9 10 
Searching for 5: Found at index 4
Searching for 1: Found at index 0
Searching for 10: Found at index 9
Searching for 15: Found at index -1

Using iterative version:
Searching for 5: Found at index 4
Searching for 15: Found at index -1
```

## Key Points

- **Time Complexity**: O(log₃ n) - Each step reduces the search space by 1/3
- **Space Complexity**: 
  - Recursive: O(log₃ n) due to call stack
  - Iterative: O(1)
- **Prerequisite**: Array must be sorted
- **Advantage**: Better than binary search for certain cases, especially when the array is very large and the cost of comparison is high

## How it works

1. Divide the array into three equal parts using two mid points
2. Compare the target with both mid points
3. Eliminate one-third of the array based on comparisons
4. Recursively or iteratively search in the remaining segments

The ternary search algorithm is particularly useful when you need to minimize the number of comparisons, as it reduces the search space more aggressively than binary search.
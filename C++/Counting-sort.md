# Counting Sort Algorithm in C++

## Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

void countingSort(vector<int>& arr) {
    // Find the maximum element to determine the range
    int maxElement = *max_element(arr.begin(), arr.end());
    
    // Create a count array to store count of each element
    vector<int> count(maxElement + 1, 0);
    
    // Store count of each element
    for (int i = 0; i < arr.size(); i++) {
        count[arr[i]]++;
    }
    
    // Modify count array to store actual position of elements
    for (int i = 1; i <= maxElement; i++) {
        count[i] += count[i - 1];
    }
    
    // Build the output array
    vector<int> output(arr.size());
    for (int i = arr.size() - 1; i >= 0; i--) {
        output[count[arr[i]] - 1] = arr[i];
        count[arr[i]]--;
    }
    
    // Copy the sorted elements back to original array
    for (int i = 0; i < arr.size(); i++) {
        arr[i] = output[i];
    }
}

// Function to print array
void printArray(const vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Main function to demonstrate counting sort
int main() {
    vector<int> arr = {4, 2, 2, 8, 3, 3, 1};
    
    cout << "Original array: ";
    printArray(arr);
    
    countingSort(arr);
    
    cout << "Sorted array: ";
    printArray(arr);
    
    return 0;
}
```

## Output
```
Original array: 4 2 2 8 3 3 1 
Sorted array: 1 2 2 3 3 4 8 
```

## How it works:

1. **Find Maximum**: Determine the range of input values
2. **Count Elements**: Count occurrences of each element
3. **Calculate Positions**: Modify count array to store actual positions
4. **Build Output**: Place elements in correct positions
5. **Copy Back**: Transfer sorted elements to original array

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is number of elements and k is range of input
- **Space Complexity**: O(k) for the count array

## Key Points:
- Counting sort is efficient when the range of possible values (k) is not significantly larger than the number of elements (n)
- It's a stable sorting algorithm
- Works only with non-negative integers
- Not comparison-based sorting algorithm


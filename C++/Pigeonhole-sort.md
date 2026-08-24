# Pigeonhole Sort Algorithm in C++

## Algorithm Overview
Pigeonhole sort is a sorting algorithm that works efficiently when the range of possible values is small compared to the number of elements to be sorted.

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void pigeonholeSort(vector<int>& arr) {
    if (arr.empty()) return;
    
    // Find the minimum and maximum values
    int min_val = *min_element(arr.begin(), arr.end());
    int max_val = *max_element(arr.begin(), arr.end());
    
    // Calculate the range
    int range = max_val - min_val + 1;
    
    // Create pigeonholes (buckets)
    vector<int> holes(range, 0);
    
    // Place elements in pigeonholes
    for (int i = 0; i < arr.size(); i++) {
        holes[arr[i] - min_val]++;
    }
    
    // Collect elements from pigeonholes back to array
    int index = 0;
    for (int i = 0; i < range; i++) {
        while (holes[i] > 0) {
            arr[index++] = i + min_val;
            holes[i]--;
        }
    }
}

// Helper function to print array
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

// Main function to demonstrate the algorithm
int main() {
    // Example 1
    vector<int> arr1 = {8, 3, 2, 7, 4, 6, 8, 3};
    cout << "Original array: ";
    printArray(arr1);
    
    pigeonholeSort(arr1);
    cout << "Sorted array:   ";
    printArray(arr1);
    
    cout << "\n";
    
    // Example 2
    vector<int> arr2 = {5, 2, 8, 1, 9, 3};
    cout << "Original array: ";
    printArray(arr2);
    
    pigeonholeSort(arr2);
    cout << "Sorted array:   ";
    printArray(arr2);
    
    return 0;
}
```

## Output
```
Original array: 8 3 2 7 4 6 8 3 
Sorted array:   2 3 3 4 6 7 8 8 

Original array: 5 2 8 1 9 3 
Sorted array:   1 2 3 5 8 9 
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of size (max - min + 1) initialized to zero
3. **Place Elements**: For each element, increment the corresponding pigeonhole counter
4. **Collect Elements**: Traverse the pigeonholes and place elements back into the original array in sorted order

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the range
- **Average Case**: O(n + k)
- **Worst Case**: O(n + k)

## Space Complexity: O(k)

## When to Use:
Pigeonhole sort is most efficient when:
- The range of possible values (k) is not significantly larger than the number of elements (n)
- The elements are integers within a known, limited range
- Memory usage is not a concern

The algorithm is particularly useful for sorting small integer arrays where the range of values is known and relatively small.
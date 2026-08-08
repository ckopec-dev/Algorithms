# Comb Sort Algorithm in C++

Comb sort is an improved version of bubble sort that uses a gap sequence to eliminate turtles (small values near the end of the array).

## Code Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Function to perform comb sort
void combSort(vector<int>& arr) {
    int n = arr.size();
    int gap = n;  // Initial gap size
    bool swapped = true;  // Flag to check if any swap occurred
    
    // Keep running while gap is more than 1 and last iteration caused a swap
    while (gap != 1 || swapped) {
        // Find next gap using shrink factor
        gap = (gap * 10) / 13;
        
        // If gap becomes less than 1, set it to 1
        if (gap < 1) {
            gap = 1;
        }
        
        swapped = false;
        
        // Compare elements with current gap
        for (int i = 0; i < n - gap; i++) {
            if (arr[i] > arr[i + gap]) {
                swap(arr[i], arr[i + gap]);
                swapped = true;
            }
        }
    }
}

// Function to print array elements
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

// Main function to demonstrate comb sort
int main() {
    // Example array to sort
    vector<int> arr = {8, 4, 1, 56, 3, -44, 23, -6, 28, 0};
    
    cout << "Original array: ";
    printArray(arr);
    
    // Apply comb sort
    combSort(arr);
    
    cout << "Sorted array:   ";
    printArray(arr);
    
    return 0;
}
```

## Output
```
Original array: 8 4 1 56 3 -44 23 -6 28 0 
Sorted array:   -44 -6 0 1 3 4 8 23 28 56 
```

## How it Works

1. **Initial Gap**: Start with a gap equal to the size of the array
2. **Gap Reduction**: Reduce the gap by dividing by 1.3 (shrink factor) in each iteration
3. **Comparison**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until gap becomes 1 and no more swaps occur

## Time Complexity
- **Best Case**: O(n log n)
- **Average Case**: O(n²/2^p) where p is the number of increments
- **Worst Case**: O(n²)

## Space Complexity
- O(1) - sorts in place

The comb sort algorithm is more efficient than bubble sort because it eliminates turtles more quickly by using larger gaps initially.
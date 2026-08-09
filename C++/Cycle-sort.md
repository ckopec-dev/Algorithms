# Cycle Sort Algorithm in C++

Cycle sort is an in-place, unstable sorting algorithm that is particularly useful when the array elements are in the range [0, n-1] where n is the size of the array.

## Code Implementation

```cpp
#include <iostream>
using namespace std;

void cycleSort(int arr[], int n) {
    // Count number of cycles
    int writes = 0;
    
    // For each position in the array
    for (int cycleStart = 0; cycleStart <= n - 2; cycleStart++) {
        int item = arr[cycleStart];
        
        // Find the position where we put the item
        int pos = cycleStart;
        for (int i = cycleStart + 1; i < n; i++) {
            if (arr[i] < item)
                pos++;
        }
        
        // If item is already in correct position
        if (pos == cycleStart)
            continue;
            
        // Otherwise, put the item at its correct position
        while (item == arr[pos]) {
            pos += 1;
        }
        
        // Swap the item with its correct position
        swap(item, arr[pos]);
        writes++;
        
        // Continue the cycle until we get back to the starting position
        while (pos != cycleStart) {
            pos = cycleStart;
            for (int i = cycleStart + 1; i < n; i++) {
                if (arr[i] < item)
                    pos++;
            }
            
            while (item == arr[pos]) {
                pos += 1;
            }
            
            swap(item, arr[pos]);
            writes++;
        }
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
    int arr[] = {4, 3, 2, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    cout << "Original array: ";
    printArray(arr, n);
    
    cycleSort(arr, n);
    
    cout << "Sorted array: ";
    printArray(arr, n);
    
    return 0;
}
```

## Output
```
Original array: 4 3 2 1 5 
Sorted array: 1 2 3 4 5 
```

## How it Works

1. **Cycle Detection**: The algorithm identifies cycles in the permutation of elements
2. **Position Finding**: For each element, it determines where it should be placed
3. **Swapping**: Elements are swapped to their correct positions
4. **Cycle Completion**: The process continues until all elements are in their correct positions

## Time and Space Complexity

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in-place

## Key Features

- **In-place sorting**: Uses only O(1) extra memory
- **Minimum number of writes**: Performs at most n-1 writes
- **Unstable**: May change the relative order of equal elements
- **Best for**: Arrays with elements in range [0, n-1]
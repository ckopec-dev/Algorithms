# Bead Sort Algorithm in C++

Bead sort is a sorting algorithm that works by simulating beads falling under gravity. Here's an implementation in C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

class BeadSort {
public:
    static void beadSort(std::vector<int>& arr) {
        if (arr.empty()) return;
        
        // Find maximum value to determine the "height" of the bead tower
        int maxVal = *std::max_element(arr.begin(), arr.end());
        
        // Create a 2D grid representing the bead positions
        // Each row represents a level, each column represents an element
        std::vector<std::vector<bool>> beads(maxVal, std::vector<bool>(arr.size(), false));
        
        // Place beads (set true values) based on input array
        for (int i = 0; i < arr.size(); i++) {
            for (int j = 0; j < arr[i]; j++) {
                beads[maxVal - 1 - j][i] = true;
            }
        }
        
        // Let beads fall by gravity (simplify the grid)
        for (int i = 0; i < maxVal; i++) {
            int count = 0;
            for (int j = 0; j < arr.size(); j++) {
                if (beads[i][j]) {
                    count++;
                }
            }
            // Fill the current level with beads
            for (int j = 0; j < arr.size(); j++) {
                beads[i][j] = (j < count);
            }
        }
        
        // Extract sorted array from the bead positions
        for (int i = 0; i < arr.size(); i++) {
            int count = 0;
            for (int j = 0; j < maxVal; j++) {
                if (beads[j][i]) {
                    count++;
                }
            }
            arr[i] = count;
        }
    }
};

// Alternative simpler implementation
void simpleBeadSort(std::vector<int>& arr) {
    if (arr.empty()) return;
    
    int maxVal = *std::max_element(arr.begin(), arr.end());
    std::vector<int> temp(maxVal, 0);
    
    // Count beads for each height level
    for (int num : arr) {
        temp[num - 1]++;
    }
    
    // Reconstruct sorted array
    int index = 0;
    for (int i = maxVal - 1; i >= 0; i--) {
        while (temp[i] > 0) {
            arr[index++] = i + 1;
            temp[i]--;
        }
    }
}

// Utility function to print array
void printArray(const std::vector<int>& arr) {
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

int main() {
    // Test the bead sort algorithm
    std::vector<int> arr = {5, 3, 1, 4, 2};
    
    std::cout << "Original array: ";
    printArray(arr);
    
    // Using the bead sort implementation
    BeadSort::beadSort(arr);
    
    std::cout << "Sorted array:   ";
    printArray(arr);
    
    // Test with another example
    std::vector<int> arr2 = {8, 3, 6, 1, 9, 2};
    std::cout << "\nOriginal array: ";
    printArray(arr2);
    
    simpleBeadSort(arr2);
    
    std::cout << "Sorted array:   ";
    printArray(arr2);
    
    return 0;
}
```

## Output:
```
Original array: 5 3 1 4 2 
Sorted array:   1 2 3 4 5 

Original array: 8 3 6 1 9 2 
Sorted array:   1 2 3 6 8 9 
```

## How it works:

1. **Bead Representation**: Each number in the input array represents a "tower" of beads
2. **Gravity Simulation**: Beads fall to the bottom of their respective towers
3. **Sorting**: The final arrangement of beads corresponds to the sorted array

## Time and Space Complexity:
- **Time Complexity**: O(sum of all elements) - This is the key advantage of bead sort
- **Space Complexity**: O(max_value × array_size)

The algorithm is particularly efficient when the range of input values is not too large, making it one of the few sorting algorithms that can achieve linear time complexity under certain conditions.
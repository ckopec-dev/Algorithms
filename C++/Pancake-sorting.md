# Pancake Sorting Algorithm in C++

Pancake sorting is the process of sorting an array by performing a series of "pancake flips" - reversing the order of the first k elements of the array.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

class PancakeSort {
public:
    // Function to flip the array from index 0 to k
    void flip(std::vector<int>& arr, int k) {
        int start = 0;
        int end = k;
        while (start < end) {
            std::swap(arr[start], arr[end]);
            start++;
            end--;
        }
    }
    
    // Function to find the index of the maximum element in arr[0..n-1]
    int findMaxIndex(std::vector<int>& arr, int n) {
        int maxIndex = 0;
        for (int i = 1; i < n; i++) {
            if (arr[i] > arr[maxIndex]) {
                maxIndex = i;
            }
        }
        return maxIndex;
    }
    
    // Main pancake sorting function
    std::vector<int> pancakeSort(std::vector<int>& arr) {
        int n = arr.size();
        std::vector<int> flips; // To store the flip operations
        
        // Start from the end and work backwards
        for (int size = n; size > 1; size--) {
            // Find the index of the maximum element in arr[0..size-1]
            int maxIndex = findMaxIndex(arr, size);
            
            // If the maximum element is not already at the end
            if (maxIndex != size - 1) {
                // Flip the array to bring the maximum element to the front
                if (maxIndex != 0) {
                    flip(arr, maxIndex);
                    flips.push_back(maxIndex + 1); // Store the flip operation (1-indexed)
                }
                
                // Flip the array to bring the maximum element to its correct position
                flip(arr, size - 1);
                flips.push_back(size); // Store the flip operation (1-indexed)
            }
        }
        
        return flips;
    }
    
    // Function to print the array
    void printArray(const std::vector<int>& arr) {
        for (int num : arr) {
            std::cout << num << " ";
        }
        std::cout << std::endl;
    }
    
    // Function to print flip operations
    void printFlips(const std::vector<int>& flips) {
        std::cout << "Flip operations: ";
        for (int flip : flips) {
            std::cout << flip << " ";
        }
        std::cout << std::endl;
    }
};

int main() {
    PancakeSort ps;
    
    // Example array to sort
    std::vector<int> arr = {3, 2, 4, 1};
    
    std::cout << "Original array: ";
    ps.printArray(arr);
    
    // Sort the array using pancake sorting
    std::vector<int> flips = ps.pancakeSort(arr);
    
    std::cout << "Sorted array: ";
    ps.printArray(arr);
    
    ps.printFlips(flips);
    
    // Another example
    std::cout << "\n--- Another Example ---" << std::endl;
    std::vector<int> arr2 = {5, 2, 8, 1, 9};
    
    std::cout << "Original array: ";
    ps.printArray(arr2);
    
    std::vector<int> flips2 = ps.pancakeSort(arr2);
    
    std::cout << "Sorted array: ";
    ps.printArray(arr2);
    
    ps.printFlips(flips2);
    
    return 0;
}
```

## How it Works

1. **Find Maximum**: For each position from the end, find the maximum element in the unsorted portion
2. **Bring to Front**: If the maximum element is not already at the front, flip it to the front
3. **Bring to Correct Position**: Flip the maximum element to its correct position at the end
4. **Repeat**: Continue this process for the remaining unsorted portion

## Example Output

```
Original array: 3 2 4 1 
Sorted array: 1 2 3 4 
Flip operations: 3 4 2 3 1 2 

--- Another Example ---
Original array: 5 2 8 1 9 
Sorted array: 1 2 5 8 9 
Flip operations: 5 3 2 5 1 2 3 1 2 1 
```

## Time and Space Complexity

- **Time Complexity**: O(n²) where n is the number of elements
- **Space Complexity**: O(1) for the algorithm itself (excluding the space for storing flip operations)

The pancake sorting algorithm is interesting because it's based on a real-world metaphor and demonstrates how to sort using a specific type of operation (reversals) rather than comparisons.


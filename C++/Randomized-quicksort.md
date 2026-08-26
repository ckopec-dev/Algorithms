# Randomized Quicksort Algorithm in C++

Here's a complete implementation of the Randomized Quicksort algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>

class RandomizedQuicksort {
private:
    // Partition function using random pivot selection
    int randomizedPartition(std::vector<int>& arr, int low, int high) {
        // Generate random index between low and high
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(low, high);
        
        int randomIndex = dis(gen);
        
        // Swap random element with last element
        std::swap(arr[randomIndex], arr[high]);
        
        // Use standard partitioning (last element as pivot)
        return partition(arr, low, high);
    }
    
    // Standard partition function
    int partition(std::vector<int>& arr, int low, int high) {
        int pivot = arr[high];  // Choose last element as pivot
        int i = low - 1;       // Index of smaller element
        
        for (int j = low; j < high; j++) {
            // If current element is smaller than or equal to pivot
            if (arr[j] <= pivot) {
                i++;
                std::swap(arr[i], arr[j]);
            }
        }
        
        // Place pivot in its correct position
        std::swap(arr[i + 1], arr[high]);
        return i + 1;
    }
    
public:
    // Main randomized quicksort function
    void quicksort(std::vector<int>& arr, int low, int high) {
        if (low < high) {
            // Partition the array and get pivot index
            int pivotIndex = randomizedPartition(arr, low, high);
            
            // Recursively sort elements before and after partition
            quicksort(arr, low, pivotIndex - 1);
            quicksort(arr, pivotIndex + 1, high);
        }
    }
    
    // Wrapper function for easier use
    void sort(std::vector<int>& arr) {
        if (!arr.empty()) {
            quicksort(arr, 0, arr.size() - 1);
        }
    }
};

// Utility function to print array
void printArray(const std::vector<int>& arr) {
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

// Example usage
int main() {
    // Test case 1
    std::vector<int> arr1 = {64, 34, 25, 12, 22, 11, 90};
    
    std::cout << "Original array: ";
    printArray(arr1);
    
    RandomizedQuicksort qs;
    qs.sort(arr1);
    
    std::cout << "Sorted array:   ";
    printArray(arr1);
    
    // Test case 2 - with duplicates
    std::vector<int> arr2 = {5, 2, 8, 2, 9, 1, 5, 5};
    
    std::cout << "\nOriginal array with duplicates: ";
    printArray(arr2);
    
    qs.sort(arr2);
    
    std::cout << "Sorted array:                   ";
    printArray(arr2);
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Random Pivot Selection**
- Uses `std::random_device` and `std::mt19937` for high-quality random number generation
- Randomly selects an index between the current range and swaps it with the last element

### 2. **Time Complexity**
- **Average Case**: O(n log n)
- **Worst Case**: O(n²) - but very unlikely due to randomization
- **Best Case**: O(n log n)

### 3. **Space Complexity**
- O(log n) due to recursive call stack

### 4. **Advantages of Randomized Quicksort**
- Eliminates worst-case performance on sorted arrays
- Provides better average performance
- More predictable behavior than standard quicksort

### Sample Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array:   11 12 22 25 34 64 90 

Original array with duplicates: 5 2 8 2 9 1 5 5 
Sorted array:                   1 2 2 5 5 5 8 9 
```

This implementation ensures that the algorithm performs well on average regardless of input distribution, making it a robust sorting solution.
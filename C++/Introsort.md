# Introsort Algorithm in C++

Introsort is a hybrid sorting algorithm that combines quicksort, heapsort, and insertion sort. It starts with quicksort, switches to heapsort when the recursion depth exceeds a certain limit, and uses insertion sort for small arrays.

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>

class Introsort {
private:
    static const int INSERTION_SORT_THRESHOLD = 16;
    static const int MAX_DEPTH = 2 * std::log2(1000); // For array of size 1000
    
    // Insertion sort for small arrays
    static void insertionSort(std::vector<int>& arr, int low, int high) {
        for (int i = low + 1; i <= high; i++) {
            int key = arr[i];
            int j = i - 1;
            
            while (j >= low && arr[j] > key) {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    }
    
    // Partition function for quicksort
    static int partition(std::vector<int>& arr, int low, int high) {
        int pivot = arr[high];
        int i = low - 1;
        
        for (int j = low; j < high; j++) {
            if (arr[j] <= pivot) {
                i++;
                std::swap(arr[i], arr[j]);
            }
        }
        std::swap(arr[i + 1], arr[high]);
        return i + 1;
    }
    
    // Heapify function for heapsort
    static void heapify(std::vector<int>& arr, int n, int i) {
        int largest = i;
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        
        if (left < n && arr[left] > arr[largest])
            largest = left;
        
        if (right < n && arr[right] > arr[largest])
            largest = right;
        
        if (largest != i) {
            std::swap(arr[i], arr[largest]);
            heapify(arr, n, largest);
        }
    }
    
    // Heapsort implementation
    static void heapSort(std::vector<int>& arr, int low, int high) {
        int n = high - low + 1;
        for (int i = n / 2 - 1; i >= 0; i--)
            heapify(arr, n, i);
        
        for (int i = n - 1; i > 0; i--) {
            std::swap(arr[low], arr[low + i]);
            heapify(arr, i, 0);
        }
    }
    
    // Introsort recursive function
    static void introsortHelper(std::vector<int>& arr, int low, int high, int depth) {
        int size = high - low + 1;
        
        // Use insertion sort for small arrays
        if (size <= INSERTION_SORT_THRESHOLD) {
            insertionSort(arr, low, high);
            return;
        }
        
        // Switch to heapsort if recursion depth exceeds limit
        if (depth == 0) {
            heapSort(arr, low, high);
            return;
        }
        
        // Perform quicksort partitioning
        int pivot = partition(arr, low, high);
        
        // Recursively sort both parts
        introsortHelper(arr, low, pivot - 1, depth - 1);
        introsortHelper(arr, pivot + 1, high, depth - 1);
    }
    
public:
    // Main introsort function
    static void introsort(std::vector<int>& arr) {
        if (arr.size() <= 1) return;
        
        int maxDepth = 2 * std::log2(arr.size());
        introsortHelper(arr, 0, arr.size() - 1, maxDepth);
    }
};

// Utility function to print array
void printArray(const std::vector<int>& arr) {
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

int main() {
    // Test with different arrays
    std::vector<int> arr1 = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42};
    std::vector<int> arr2 = {5, 2, 8, 1, 9, 3, 7, 4, 6};
    std::vector<int> arr3 = {100, 50, 75, 25, 12, 37, 62, 87, 90, 15};
    
    std::cout << "Original array 1: ";
    printArray(arr1);
    
    Introsort::introsort(arr1);
    
    std::cout << "Sorted array 1:   ";
    printArray(arr1);
    
    std::cout << "\nOriginal array 2: ";
    printArray(arr2);
    
    Introsort::introsort(arr2);
    
    std::cout << "Sorted array 2:   ";
    printArray(arr2);
    
    std::cout << "\nOriginal array 3: ";
    printArray(arr3);
    
    Introsort::introsort(arr3);
    
    std::cout << "Sorted array 3:   ";
    printArray(arr3);
    
    // Performance test with larger array
    std::cout << "\nPerformance test with 1000 random elements:" << std::endl;
    std::vector<int> largeArr(1000);
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 10000);
    
    for (int& num : largeArr) {
        num = dis(gen);
    }
    
    auto start = std::chrono::high_resolution_clock::now();
    Introsort::introsort(largeArr);
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    std::cout << "Time taken: " << duration.count() << " microseconds" << std::endl;
    
    // Verify sorting
    bool isSorted = std::is_sorted(largeArr.begin(), largeArr.end());
    std::cout << "Array is sorted: " << (isSorted ? "Yes" : "No") << std::endl;
    
    return 0;
}
```

## Key Features of this Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Limit**: Uses a maximum recursion depth to switch to heapsort
3. **Small Array Optimization**: Uses insertion sort for arrays smaller than threshold
4. **Time Complexity**: O(n log n) average case, O(n log n) worst case
5. **Space Complexity**: O(log n) due to recursion stack

## How it Works:

1. **Initial Phase**: Uses quicksort for partitioning
2. **Depth Monitoring**: Tracks recursion depth to prevent worst-case scenarios
3. **Switch to Heapsort**: When depth limit is exceeded, switches to heapsort
4. **Small Arrays**: Uses insertion sort for small subarrays (≤ 16 elements)

This implementation provides a robust sorting algorithm that performs well on various types of input data while maintaining guaranteed O(n log n) time complexity.


# TimSort Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <chrono>

class TimSort {
private:
    static const int MIN_MERGE = 32;
    
    // Function to find the minimum run length
    static int minRunLength(int n) {
        int r = 0;
        while (n >= MIN_MERGE) {
            r |= (n & 1);
            n >>= 1;
        }
        return n + r;
    }
    
    // Insertion sort for small arrays
    static void insertionSort(std::vector<int>& arr, int left, int right) {
        for (int i = left + 1; i <= right; i++) {
            int key = arr[i];
            int j = i - 1;
            
            while (j >= left && arr[j] > key) {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    }
    
    // Merge two sorted subarrays
    static void merge(std::vector<int>& arr, int left, int mid, int right) {
        int len1 = mid - left + 1;
        int len2 = right - mid;
        
        std::vector<int> leftArr(len1);
        std::vector<int> rightArr(len2);
        
        for (int i = 0; i < len1; i++)
            leftArr[i] = arr[left + i];
        for (int i = 0; i < len2; i++)
            rightArr[i] = arr[mid + 1 + i];
        
        int i = 0, j = 0, k = left;
        
        while (i < len1 && j < len2) {
            if (leftArr[i] <= rightArr[j]) {
                arr[k] = leftArr[i];
                i++;
            } else {
                arr[k] = rightArr[j];
                j++;
            }
            k++;
        }
        
        while (i < len1) {
            arr[k] = leftArr[i];
            i++;
            k++;
        }
        
        while (j < len2) {
            arr[k] = rightArr[j];
            j++;
            k++;
        }
    }
    
public:
    // Main TimSort function
    static void timSort(std::vector<int>& arr) {
        int n = arr.size();
        
        if (n < 2) return;
        
        // Find minimum run length
        int minRun = minRunLength(n);
        
        // Sort individual runs using insertion sort
        for (int i = 0; i < n; i += minRun) {
            insertionSort(arr, i, std::min(i + minRun - 1, n - 1));
        }
        
        // Merge runs
        for (int size = minRun; size < n; size = 2 * size) {
            for (int left = 0; left < n - size; left += 2 * size) {
                int mid = left + size - 1;
                int right = std::min(left + 2 * size - 1, n - 1);
                
                if (mid < right) {
                    merge(arr, left, mid, right);
                }
            }
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
    // Test case 1: Random array
    std::vector<int> arr1 = {5, 21, 7, 23, 19, 25, 1, 18, 3, 12, 6, 14, 8, 16, 10, 13, 11, 15, 9, 17};
    
    std::cout << "Original array:" << std::endl;
    printArray(arr1);
    
    TimSort::timSort(arr1);
    
    std::cout << "Sorted array:" << std::endl;
    printArray(arr1);
    
    // Test case 2: Already sorted array
    std::vector<int> arr2 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    std::cout << "\nAlready sorted array:" << std::endl;
    printArray(arr2);
    
    TimSort::timSort(arr2);
    
    std::cout << "Sorted array:" << std::endl;
    printArray(arr2);
    
    // Test case 3: Reverse sorted array
    std::vector<int> arr3 = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
    
    std::cout << "\nReverse sorted array:" << std::endl;
    printArray(arr3);
    
    TimSort::timSort(arr3);
    
    std::cout << "Sorted array:" << std::endl;
    printArray(arr3);
    
    return 0;
}
```

## How TimSort Works

TimSort is a hybrid stable sorting algorithm that combines:
1. **Insertion Sort** for small arrays (typically 32 elements or less)
2. **Merge Sort** for larger arrays

### Key Features:
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data
- **Hybrid approach**: Uses insertion sort for small runs and merge sort for combining runs
- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
- **Space Complexity**: O(n)

### Algorithm Steps:
1. **Find runs**: Identify naturally occurring sorted subsequences
2. **Insertion sort**: Sort small runs using insertion sort
3. **Merge runs**: Combine runs using merge operation, but merge smaller runs first to larger ones

### Output Example:
```
Original array:
5 21 7 23 19 25 1 18 3 12 6 14 8 16 10 13 11 15 9 17 
Sorted array:
1 3 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 21 23 25 

Already sorted array:
1 2 3 4 5 6 7 8 9 10 
Sorted array:
1 2 3 4 5 6 7 8 9 10 

Reverse sorted array:
10 9 8 7 6 5 4 3 2 1 
Sorted array:
1 2 3 4 5 6 7 8 9 10 
```


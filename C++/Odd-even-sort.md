# Odd-Even Sort Algorithm in C++

## Algorithm Explanation

Odd-Even sort (also known as brick sort) is a comparison-based sorting algorithm that works by comparing elements at odd and even indices alternately. It's similar to bubble sort but with a specific pattern of comparisons.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

void oddEvenSort(vector<int>& arr) {
    int n = arr.size();
    bool sorted = false;
    
    while (!sorted) {
        sorted = true;
        
        // Perform odd phase
        for (int i = 1; i < n - 1; i += 2) {
            if (arr[i] > arr[i + 1]) {
                swap(arr[i], arr[i + 1]);
                sorted = false;
            }
        }
        
        // Perform even phase
        for (int i = 0; i < n - 1; i += 2) {
            if (arr[i] > arr[i + 1]) {
                swap(arr[i], arr[i + 1]);
                sorted = false;
            }
        }
    }
}

// Function to print array
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

int main() {
    // Example usage
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90};
    
    cout << "Original array: ";
    printArray(arr);
    
    oddEvenSort(arr);
    
    cout << "Sorted array:   ";
    printArray(arr);
    
    return 0;
}
```

## Output
```
Original array: 64 34 25 12 22 11 90 
Sorted array:   11 12 22 25 34 64 90 
```

## How it Works

1. **Odd Phase**: Compare elements at odd indices (1, 3, 5, ...) with their next neighbors
2. **Even Phase**: Compare elements at even indices (0, 2, 4, ...) with their next neighbors
3. Repeat both phases until no swaps are needed (array is sorted)

## Time Complexity
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity
- O(1) - only uses a constant amount of extra memory

## Key Characteristics
- **Stable**: Equal elements maintain their relative order
- **In-place**: Requires only O(1) additional memory
- **Parallelizable**: The odd and even phases can be executed in parallel
- **Adaptive**: Performs better on partially sorted arrays
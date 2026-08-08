# Cocktail Shaker Sort Algorithm in C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

void cocktailShakerSort(vector<int>& arr) {
    int left = 0;
    int right = arr.size() - 1;
    bool swapped = true;
    
    while (swapped) {
        swapped = false;
        
        // Forward pass (left to right)
        for (int i = left; i < right; i++) {
            if (arr[i] > arr[i + 1]) {
                swap(arr[i], arr[i + 1]);
                swapped = true;
            }
        }
        
        // Reduce the right boundary since the largest element is now in place
        right--;
        
        if (!swapped) break;
        
        // Backward pass (right to left)
        for (int i = right; i > left; i--) {
            if (arr[i] < arr[i - 1]) {
                swap(arr[i], arr[i - 1]);
                swapped = true;
            }
        }
        
        // Increase the left boundary since the smallest element is now in place
        left++;
    }
}

// Function to print the array
void printArray(const vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

int main() {
    // Example usage
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90};
    
    cout << "Original array: ";
    printArray(arr);
    
    cocktailShakerSort(arr);
    
    cout << "Sorted array:   ";
    printArray(arr);
    
    return 0;
}
```

## Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array:   11 12 22 25 34 64 90 
```

## How it works:

1. **Bidirectional Sorting**: Unlike traditional bubble sort, cocktail shaker sort moves in both directions alternately
2. **Forward Pass**: From left to right, pushing the largest element to the right end
3. **Backward Pass**: From right to left, pushing the smallest element to the left end
4. **Boundary Adjustment**: After each pass, the sorted boundaries are adjusted
5. **Early Termination**: If no swaps occur in a complete pass, the array is sorted

## Time Complexity:
- Best Case: O(n) - when array is already sorted
- Average Case: O(n²)
- Worst Case: O(n²)

## Space Complexity: O(1) - sorts in place
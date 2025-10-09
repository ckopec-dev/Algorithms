# Pigeonhole Sort Algorithm in C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

void pigeonholeSort(vector<int>& arr) {
    if (arr.empty()) return;
    
    // Find the minimum and maximum values
    int min_val = arr[0];
    int max_val = arr[0];
    
    for (int i = 1; i < arr.size(); i++) {
        if (arr[i] < min_val) min_val = arr[i];
        if (arr[i] > max_val) max_val = arr[i];
    }
    
    // Calculate the range
    int range = max_val - min_val + 1;
    
    // Create pigeonholes
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

// Utility function to print array
void printArray(const vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

int main() {
    // Example usage
    vector<int> arr = {8, 3, 2, 7, 4, 6, 1, 5};
    
    cout << "Original array: ";
    printArray(arr);
    
    pigeonholeSort(arr);
    
    cout << "Sorted array:   ";
    printArray(arr);
    
    return 0;
}
```

## Output:
```
Original array: 8 3 2 7 4 6 1 5 
Sorted array:   1 2 3 4 5 6 7 8 
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of size (max - min + 1) initialized to 0
3. **Place Elements**: For each element in the original array, increment the corresponding pigeonhole
4. **Collect Elements**: Traverse the pigeonholes and place elements back in the original array in sorted order

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the range
- **Average Case**: O(n + k)
- **Worst Case**: O(n + k)

## Space Complexity: O(k)

## When to Use:
- When the range of possible values (k) is not significantly larger than the number of elements (n)
- When sorting integers or discrete values
- When you need stable sorting with linear time complexity


# Bucket Sort Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void bucketSort(vector<float>& arr) {
    int n = arr.size();
    
    // Create n empty buckets
    vector<vector<float>> buckets(n);
    
    // Put array elements in different buckets
    for (int i = 0; i < n; i++) {
        int bucketIndex = n * arr[i]; // Index in bucket
        buckets[bucketIndex].push_back(arr[i]);
    }
    
    // Sort individual buckets
    for (int i = 0; i < n; i++) {
        sort(buckets[i].begin(), buckets[i].end());
    }
    
    // Concatenate all buckets into arr
    int index = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < buckets[i].size(); j++) {
            arr[index++] = buckets[i][j];
        }
    }
}

// Utility function to print array
void printArray(vector<float>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

int main() {
    // Example usage
    vector<float> arr = {0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434};
    
    cout << "Original array: ";
    printArray(arr);
    
    bucketSort(arr);
    
    cout << "Sorted array: ";
    printArray(arr);
    
    return 0;
}
```

## Output:
```
Original array: 0.897 0.565 0.656 0.1234 0.665 0.3434 
Sorted array: 0.1234 0.3434 0.565 0.656 0.665 0.897 
```

## How it works:

1. **Create Buckets**: Create n empty buckets (where n is the number of elements)
2. **Distribute Elements**: Place each element in the appropriate bucket based on its value
3. **Sort Buckets**: Sort individual buckets using a sorting algorithm (typically insertion sort)
4. **Concatenate**: Combine all sorted buckets to get the final sorted array

## Time Complexity:
- **Best Case**: O(n + k) - when elements are uniformly distributed
- **Average Case**: O(n + k) 
- **Worst Case**: O(n²) - when all elements are placed in one bucket

## Space Complexity: O(n × k)

## Key Points:
- Bucket sort is a distribution sort
- Works best when input is uniformly distributed
- Stable sorting algorithm
- Requires additional space for buckets


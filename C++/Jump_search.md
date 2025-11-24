# Jump Search Algorithm in C++

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified block.

## Code Implementation

```cpp
#include <iostream>
#include <cmath>
using namespace std;

int jumpSearch(int arr[], int n, int target) {
    // Size of block to be jumped
    int step = sqrt(n);
    
    // Find the block where element is present
    int prev = 0;
    while (arr[min(step, n) - 1] < target) {
        prev = step;
        step += sqrt(n);
        if (prev >= n)
            return -1;
    }
    
    // Linear search in the identified block
    while (arr[prev] < target) {
        prev++;
        if (prev == min(step, n))
            return -1;
    }
    
    // If element is found
    if (arr[prev] == target)
        return prev;
    
    return -1;
}

int main() {
    int arr[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target = 55;
    
    int result = jumpSearch(arr, n, target);
    
    if (result != -1)
        cout << "Element found at index: " << result << endl;
    else
        cout << "Element not found" << endl;
    
    return 0;
}
```

## How it works:

1. **Calculate block size**: Jump by `sqrt(n)` steps
2. **Find the block**: Keep jumping until we find a block where `arr[i] >= target`
3. **Linear search**: Perform linear search within that block
4. **Return result**: Return index if found, otherwise -1

## Output:
```
Element found at index: 10
```

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity: O(1)

## Advantages:
- More efficient than linear search for large arrays
- Simpler than binary search
- Works well with arrays that are not too large

## Disadvantages:
- Requires sorted array
- Not as efficient as binary search for very large datasets


# Jump Search Algorithm in C++

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the range where the target element might be located.

## Implementation

```cpp
#include <iostream>
#include <cmath>
using namespace std;

int jumpSearch(int arr[], int x, int n) {
    // Finding block size to be jumped
    int step = sqrt(n);
    
    // Finding the block where element is present (if it is present)
    int prev = 0;
    while (arr[min(step, n) - 1] < x) {
        prev = step;
        step += sqrt(n);
        if (prev >= n)
            return -1;
    }
    
    // Doing a linear search for x in block beginning with prev
    while (arr[prev] < x) {
        prev++;
        if (prev == min(step, n))
            return -1;
    }
    
    // If element is found
    if (arr[prev] == x)
        return prev;
    
    return -1;
}

int main() {
    int arr[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610};
    int n = sizeof(arr) / sizeof(arr[0]);
    int x = 55;
    
    int index = jumpSearch(arr, x, n);
    
    if (index != -1)
        cout << "Element " << x << " is at index " << index << endl;
    else
        cout << "Element " << x << " not found" << endl;
    
    return 0;
}
```

## Output
```
Element 55 is at index 10
```

## How it works:

1. **Calculate block size**: The optimal block size is √n (square root of array size)
2. **Jump ahead**: Start from index 0 and jump by block size until we find an element greater than or equal to the target
3. **Linear search**: Perform linear search in the identified block
4. **Return result**: Return index if found, otherwise return -1

## Time Complexity
- **Best Case**: O(1) - Element found at the first position
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity
- O(1) - Only uses a constant amount of extra space

## Advantages
- More efficient than linear search for large sorted arrays
- Simpler than binary search implementation
- Works well when the array is sorted

## Disadvantages
- Requires the array to be sorted
- Not as efficient as binary search in terms of time complexity
- May require more comparisons than binary search in some cases


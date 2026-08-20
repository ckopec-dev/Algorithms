# Linear Search Algorithm in C++

## Code Example

```cpp
#include <iostream>
using namespace std;

// Linear search function
int linearSearch(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i;  // Return index if element found
        }
    }
    return -1;  // Return -1 if element not found
}

int main() {
    int arr[] = {10, 25, 3, 47, 15, 8, 92, 33};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target = 47;
    
    cout << "Array: ";
    for (int i = 0; i < size; i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
    
    int result = linearSearch(arr, size, target);
    
    if (result != -1) {
        cout << "Element " << target << " found at index " << result << endl;
    } else {
        cout << "Element " << target << " not found in array" << endl;
    }
    
    return 0;
}
```

## Output
```
Array: 10 25 3 47 15 8 92 33 
Element 47 found at index 3
```

## How it works:
1. **Start from the first element** of the array
2. **Compare the target element** with each element sequentially
3. **If match found**, return the index of that element
4. **If no match found** after checking all elements, return -1

## Time Complexity: O(n)
## Space Complexity: O(1)

The linear search algorithm checks each element one by one until it finds the target or reaches the end of the array.
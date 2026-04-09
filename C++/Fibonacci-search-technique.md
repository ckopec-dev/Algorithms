# Fibonacci Search Technique in C++

Here's a complete implementation of the Fibonacci search algorithm in C++:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class FibonacciSearch {
public:
    // Function to find the smallest Fibonacci number greater than or equal to n
    static int findFibonacci(int n) {
        int fib1 = 0, fib2 = 1, fib = fib1 + fib2;
        
        while (fib < n) {
            fib1 = fib2;
            fib2 = fib;
            fib = fib1 + fib2;
        }
        
        return fib;
    }
    
    // Fibonacci search algorithm
    static int fibonacciSearch(const vector<int>& arr, int target) {
        int n = arr.size();
        
        // Find the smallest Fibonacci number greater than or equal to n
        int fibM = findFibonacci(n);
        
        // Mark the eliminated range from front
        int offset = -1;
        
        // While there are elements to be checked
        while (fibM > 1) {
            // Check if fibM-2 is a valid location
            int i = min(offset + fibM - 2, n - 1);
            
            // If target is greater than the value at index fibM-2,
            // cut the subarray from offset to i
            if (arr[i] < target) {
                fibM = fibM - 1;
                offset = i;
            }
            // If target is less than the value at index fibM-2,
            // cut the subarray after i
            else if (arr[i] > target) {
                fibM = fibM - 2;
            }
            // If target is found
            else {
                return i;
            }
        }
        
        // Compare the last element with target
        if (fibM == 1 && offset + 1 < n && arr[offset + 1] == target) {
            return offset + 1;
        }
        
        // Element not found
        return -1;
    }
};

// Example usage
int main() {
    // Sorted array for search
    vector<int> arr = {10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100};
    
    cout << "Array: ";
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl << endl;
    
    // Test cases
    int targets[] = {22, 85, 100, 55, 10};
    int numTargets = sizeof(targets) / sizeof(targets[0]);
    
    for (int i = 0; i < numTargets; i++) {
        int target = targets[i];
        int result = FibonacciSearch::fibonacciSearch(arr, target);
        
        if (result != -1) {
            cout << "Element " << target << " found at index: " << result << endl;
        } else {
            cout << "Element " << target << " not found in array" << endl;
        }
    }
    
    return 0;
}
```

## Output:
```
Array: 10 22 35 40 45 50 80 82 85 90 100 

Element 22 found at index: 1
Element 85 found at index: 8
Element 100 found at index: 10
Element 55 not found in array
Element 10 found at index: 0
```

## How it works:

1. **Fibonacci Sequence Generation**: The algorithm generates Fibonacci numbers until it finds one that is greater than or equal to the array size.

2. **Search Process**: 
   - Compare the target with the element at position `offset + fibM - 2`
   - If target is greater, move the Fibonacci numbers two steps back
   - If target is smaller, move the Fibonacci numbers one step back
   - If target matches, return the index

3. **Time Complexity**: O(log n)
4. **Space Complexity**: O(1)

## Key Features:
- Works only on sorted arrays
- Uses only addition and subtraction operations
- More efficient than binary search in some cases due to fewer comparisons
- Uses Fibonacci numbers to determine search boundaries


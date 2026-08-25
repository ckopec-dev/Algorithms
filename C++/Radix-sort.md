# Radix Sort Algorithm in C++

Here's a complete implementation of the Radix Sort algorithm in C++:

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Function to get the maximum value in the array
int getMax(vector<int>& arr) {
    int max = arr[0];
    for (int i = 1; i < arr.size(); i++) {
        if (arr[i] > max)
            max = arr[i];
    }
    return max;
}

// Function to do counting sort based on the digit represented by exp
void countingSort(vector<int>& arr, int exp) {
    int n = arr.size();
    vector<int> output(n); // output array
    int count[10] = {0};   // count array to store count of occurrences
    
    // Store count of occurrences in count[]
    for (int i = 0; i < n; i++) {
        count[(arr[i] / exp) % 10]++;
    }
    
    // Change count[i] to actual position
    for (int i = 1; i < 10; i++) {
        count[i] += count[i - 1];
    }
    
    // Build the output array
    for (int i = n - 1; i >= 0; i--) {
        output[count[(arr[i] / exp) % 10] - 1] = arr[i];
        count[(arr[i] / exp) % 10]--;
    }
    
    // Copy the output array to arr[], so that arr[] now contains sorted numbers
    for (int i = 0; i < n; i++) {
        arr[i] = output[i];
    }
}

// Main function to implement Radix Sort
void radixSort(vector<int>& arr) {
    // Find the maximum number to know number of digits
    int max = getMax(arr);
    
    // Do counting sort for every digit
    for (int exp = 1; max / exp > 0; exp *= 10) {
        countingSort(arr, exp);
    }
}

// Function to print the array
void printArray(vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Driver code
int main() {
    vector<int> arr = {170, 45, 75, 90, 2, 802, 24, 66};
    
    cout << "Original array: ";
    printArray(arr);
    
    radixSort(arr);
    
    cout << "Sorted array: ";
    printArray(arr);
    
    return 0;
}
```

## Output:
```
Original array: 170 45 75 90 2 802 24 66 
Sorted array: 2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine the number of digits.

2. **Counting Sort for Each Digit**: Starting from the least significant digit (units place), we perform counting sort for each digit position.

3. **Repeat**: We continue this process for all digit positions until we've processed the most significant digit.

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where:
- `n` = number of elements
- `d` = number of digits in the maximum number

## Space Complexity: O(n + k)
Where `k` is the range of input (typically 10 for decimal numbers).

## Key Features:
- **Stable**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use comparison operators
- **Efficient**: Particularly good for sorting integers with limited range
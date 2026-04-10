# BogoSort Algorithm in C++

BogoSort (also known as "stupid sort") is a highly inefficient sorting algorithm that works by randomly shuffling the array until it happens to be sorted.

```cpp
#include <iostream>
#include <algorithm>
#include <random>
#include <chrono>

// Function to check if array is sorted
bool isSorted(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        if (arr[i] > arr[i + 1]) {
            return false;
        }
    }
    return true;
}

// Function to shuffle array randomly
void shuffle(int arr[], int n) {
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(arr, arr + n, g);
}

// BogoSort algorithm
void bogoSort(int arr[], int n) {
    // Keep shuffling until array is sorted
    while (!isSorted(arr, n)) {
        shuffle(arr, n);
    }
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
}

// Main function
int main() {
    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    std::cout << "Original array: ";
    printArray(arr, n);
    
    bogoSort(arr, n);
    
    std::cout << "Sorted array: ";
    printArray(arr, n);
    
    return 0;
}
```

## How it works:

1. **Check if sorted**: The `isSorted()` function checks if the array is in ascending order
2. **Shuffle**: The `shuffle()` function randomly rearranges the elements using `std::shuffle`
3. **Repeat**: The `bogoSort()` function continues shuffling until `isSorted()` returns true

## Example Output:
```
Original array: 3 1 4 1 5 9 2 6 
Sorted array: 1 1 2 3 4 5 6 9 
```

## Important Notes:

- **Time Complexity**: O((n+1)!) in worst case, O(n) in best case
- **Space Complexity**: O(1)
- **Warning**: This algorithm is extremely inefficient and should never be used in practice
- **Probability**: The chance of randomly sorting an array of n elements is 1/n!
- **Use case**: Only for educational purposes to demonstrate inefficient algorithms


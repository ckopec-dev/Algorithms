# Linear Search Algorithm in C

## Code Example

```c
#include <stdio.h>

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
    
    printf("Array: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    int result = linearSearch(arr, size, target);
    
    if (result != -1) {
        printf("Element %d found at index %d\n", target, result);
    } else {
        printf("Element %d not found in array\n", target);
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
1. The function iterates through each element of the array
2. Compares each element with the target value
3. Returns the index if found, or -1 if not found
4. Time complexity: O(n)
5. Space complexity: O(1)


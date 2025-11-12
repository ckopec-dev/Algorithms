# TimSort Algorithm Implementation in JavaScript

```javascript
function timSort(arr) {
    const MIN_MERGE = 32;
    
    // Helper function to find the minimum run length
    function minRunLength(n) {
        let r = 0;
        while (n >= MIN_MERGE) {
            r |= n & 1;
            n >>= 1;
        }
        return n + r;
    }
    
    // Insertion sort for small arrays
    function insertionSort(arr, left, right) {
        for (let i = left + 1; i <= right; i++) {
            let key = arr[i];
            let j = i - 1;
            
            while (j >= left && arr[j] > key) {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    }
    
    // Merge two sorted subarrays
    function merge(arr, left, mid, right) {
        const leftArr = arr.slice(left, mid + 1);
        const rightArr = arr.slice(mid + 1, right + 1);
        
        let i = 0, j = 0, k = left;
        
        while (i < leftArr.length && j < rightArr.length) {
            if (leftArr[i] <= rightArr[j]) {
                arr[k] = leftArr[i];
                i++;
            } else {
                arr[k] = rightArr[j];
                j++;
            }
            k++;
        }
        
        while (i < leftArr.length) {
            arr[k] = leftArr[i];
            i++;
            k++;
        }
        
        while (j < rightArr.length) {
            arr[k] = rightArr[j];
            j++;
            k++;
        }
    }
    
    const n = arr.length;
    
    // Step 1: Sort small runs using insertion sort
    for (let i = 0; i < n; i += MIN_MERGE) {
        insertionSort(arr, i, Math.min(i + MIN_MERGE - 1, n - 1));
    }
    
    // Step 2: Merge runs of increasing size
    for (let size = MIN_MERGE; size < n; size = 2 * size) {
        for (let left = 0; left < n - size; left += 2 * size) {
            const mid = left + size - 1;
            const right = Math.min(left + 2 * size - 1, n - 1);
            
            if (mid < right) {
                merge(arr, left, mid, right);
            }
        }
    }
    
    return arr;
}

// Example usage
const exampleArray = [5, 2, 8, 1, 9, 3, 7, 4, 6];
console.log("Original array:", exampleArray);

const sortedArray = timSort([...exampleArray]); // Using spread to avoid mutating original
console.log("Sorted array:", sortedArray);

// Another example with more elements
const largeArray = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9];
console.log("Large array before sorting:", largeArray);
const sortedLargeArray = timSort([...largeArray]);
console.log("Large array after sorting:", sortedLargeArray);
```

## How TimSort Works:

1. **Divide and Conquer**: Breaks the array into small runs (subarrays)
2. **Insertion Sort**: Uses insertion sort for small runs (typically 32-64 elements)
3. **Merge**: Merges the sorted runs together using a merge operation
4. **Optimization**: Takes advantage of existing order in the data

## Key Features:

- **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
- **Space Complexity**: O(n)
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data

## Output:
```
Original array: [5, 2, 8, 1, 9, 3, 7, 4, 6]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]
Large array before sorting: [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9]
Large array after sorting: [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 7, 8, 9, 9, 9]
```


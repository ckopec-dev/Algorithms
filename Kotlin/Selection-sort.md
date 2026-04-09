# Selection Sort Algorithm in Kotlin

Here's an implementation of the selection sort algorithm in Kotlin:

```kotlin
fun selectionSort(arr: IntArray) {
    val n = arr.size
    
    // Traverse through all array elements
    for (i in 0 until n - 1) {
        // Find the minimum element in the remaining unsorted array
        var minIndex = i
        for (j in i + 1 until n) {
            if (arr[j] < arr[minIndex]) {
                minIndex = j
            }
        }
        
        // Swap the found minimum element with the first element
        if (minIndex != i) {
            val temp = arr[i]
            arr[i] = arr[minIndex]
            arr[minIndex] = temp
        }
    }
}

// Example usage
fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    
    println("Original array: ${arr.joinToString(", ")}")
    
    selectionSort(arr)
    
    println("Sorted array: ${arr.joinToString(", ")}")
}
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Find the minimum**: In each iteration, find the smallest element in the unsorted portion of the array
2. **Swap**: Place this minimum element at the beginning of the unsorted portion
3. **Move boundary**: Reduce the unsorted portion by one element
4. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in-place

The selection sort algorithm works by repeatedly finding the minimum element from the unsorted part and putting it at the beginning, making it a simple but inefficient sorting algorithm for large datasets.


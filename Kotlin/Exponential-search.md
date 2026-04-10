# Exponential Search Algorithm in Kotlin

Exponential search is a searching algorithm that works by first finding a range where the target element might exist, then performing a binary search within that range.

## Implementation

```kotlin
fun exponentialSearch(arr: IntArray, target: Int): Int {
    // If target is at the first position
    if (arr[0] == target) {
        return 0
    }
    
    // Find range for binary search by repeated doubling
    var i = 1
    while (i < arr.size && arr[i] <= target) {
        i *= 2
    }
    
    // Perform binary search in the range [i/2, min(i, arr.size-1)]
    return binarySearch(arr, target, i / 2, Math.min(i, arr.size - 1))
}

fun binarySearch(arr: IntArray, target: Int, left: Int, right: Int): Int {
    var leftIndex = left
    var rightIndex = right
    
    while (leftIndex <= rightIndex) {
        val mid = leftIndex + (rightIndex - leftIndex) / 2
        
        when {
            arr[mid] == target -> return mid
            arr[mid] < target -> leftIndex = mid + 1
            else -> rightIndex = mid - 1
        }
    }
    
    return -1 // Element not found
}

// Example usage
fun main() {
    val arr = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    
    println("Array: ${arr.contentToString()}")
    
    // Test cases
    val targets = listOf(5, 1, 15, 20)
    
    targets.forEach { target ->
        val result = exponentialSearch(arr, target)
        if (result != -1) {
            println("Element $target found at index $result")
        } else {
            println("Element $target not found")
        }
    }
}
```

## Output
```
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
Element 5 found at index 4
Element 1 found at index 0
Element 15 found at index 14
Element 20 not found
```

## How it works:

1. **First Step**: Check if the target is at index 0
2. **Range Finding**: Keep doubling the index until we find an element greater than or equal to target
3. **Binary Search**: Perform binary search in the range [i/2, min(i, arr.size-1)]

## Time Complexity:
- **Best Case**: O(1) - when element is at index 0
- **Average/Worst Case**: O(log n) - where n is the size of the array

## Space Complexity:
- O(1) - iterative implementation
- O(log n) - if recursive binary search is used

## When to use:
- When the array is sorted and unbounded or when the size is unknown
- When the target element is likely to be near the beginning of the array
- When you want to avoid the overhead of binary search in unknown-sized arrays


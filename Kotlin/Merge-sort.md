```kotlin
fun mergeSort(arr: IntArray): IntArray {
    // Base case: arrays with 0 or 1 element are already sorted
    if (arr.size <= 1) {
        return arr
    }
    
    // Divide the array into two halves
    val mid = arr.size / 2
    val leftArray = arr.copyOfRange(0, mid)
    val rightArray = arr.copyOfRange(mid, arr.size)
    
    // Recursively sort both halves
    val sortedLeft = mergeSort(leftArray)
    val sortedRight = mergeSort(rightArray)
    
    // Merge the sorted halves
    return merge(sortedLeft, sortedRight)
}

fun merge(left: IntArray, right: IntArray): IntArray {
    val result = mutableListOf<Int>()
    var leftIndex = 0
    var rightIndex = 0
    
    // Compare elements from both arrays and merge them in sorted order
    while (leftIndex < left.size && rightIndex < right.size) {
        if (left[leftIndex] <= right[rightIndex]) {
            result.add(left[leftIndex])
            leftIndex++
        } else {
            result.add(right[rightIndex])
            rightIndex++
        }
    }
    
    // Add remaining elements from left array (if any)
    while (leftIndex < left.size) {
        result.add(left[leftIndex])
        leftIndex++
    }
    
    // Add remaining elements from right array (if any)
    while (rightIndex < right.size) {
        result.add(right[rightIndex])
        rightIndex++
    }
    
    return result.toIntArray()
}

// Example usage
fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    println("Original array: ${arr.joinToString(", ")}")
    
    val sortedArr = mergeSort(arr)
    println("Sorted array: ${sortedArr.joinToString(", ")}")
}
```

**Output:**
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

**How it works:**
1. **Divide**: Split the array into two halves recursively until we have arrays of size 1
2. **Conquer**: Each single-element array is already sorted
3. **Combine**: Merge the sorted halves back together in sorted order

**Time Complexity:** O(n log n)  
**Space Complexity:** O(n)


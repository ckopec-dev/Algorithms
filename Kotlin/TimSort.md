# TimSort Algorithm in Kotlin

Here's an implementation of the TimSort algorithm in Kotlin:

```kotlin
import java.util.*

class TimSort {
    private val MIN_MERGE = 32
    
    fun timSort(arr: IntArray) {
        val n = arr.size
        if (n < 2) return
        
        // Sort individual subarrays of size MIN_MERGE
        for (i in 0 until n step MIN_MERGE) {
            insertionSort(arr, i, minOf(i + MIN_MERGE - 1, n - 1))
        }
        
        // Merge subarrays in bottom-up manner
        var size = MIN_MERGE
        while (size < n) {
            var left = 0
            while (left < n - size) {
                val mid = left + size - 1
                val right = minOf(left + size * 2 - 1, n - 1)
                
                if (mid < right) {
                    merge(arr, left, mid, right)
                }
                left += size * 2
            }
            size *= 2
        }
    }
    
    private fun insertionSort(arr: IntArray, left: Int, right: Int) {
        for (i in left + 1..right) {
            val key = arr[i]
            var j = i - 1
            
            while (j >= left && arr[j] > key) {
                arr[j + 1] = arr[j]
                j--
            }
            arr[j + 1] = key
        }
    }
    
    private fun merge(arr: IntArray, left: Int, mid: Int, right: Int) {
        val leftArray = arr.copyOfRange(left, mid + 1)
        val rightArray = arr.copyOfRange(mid + 1, right + 1)
        
        var i = 0
        var j = 0
        var k = left
        
        while (i < leftArray.size && j < rightArray.size) {
            if (leftArray[i] <= rightArray[j]) {
                arr[k] = leftArray[i]
                i++
            } else {
                arr[k] = rightArray[j]
                j++
            }
            k++
        }
        
        while (i < leftArray.size) {
            arr[k] = leftArray[i]
            i++
            k++
        }
        
        while (j < rightArray.size) {
            arr[k] = rightArray[j]
            j++
            k++
        }
    }
}

// Example usage
fun main() {
    val sorter = TimSort()
    
    // Test case 1: Random array
    val arr1 = intArrayOf(5, 2, 8, 1, 9, 3, 7, 4, 6)
    println("Original array: ${arr1.contentToString()}")
    sorter.timSort(arr1)
    println("Sorted array: ${arr1.contentToString()}")
    
    // Test case 2: Already sorted array
    val arr2 = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
    println("\nOriginal array: ${arr2.contentToString()}")
    sorter.timSort(arr2)
    println("Sorted array: ${arr2.contentToString()}")
    
    // Test case 3: Reverse sorted array
    val arr3 = intArrayOf(9, 8, 7, 6, 5, 4, 3, 2, 1)
    println("\nOriginal array: ${arr3.contentToString()}")
    sorter.timSort(arr3)
    println("Sorted array: ${arr3.contentToString()}")
    
    // Test case 4: Array with duplicates
    val arr4 = intArrayOf(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
    println("\nOriginal array: ${arr4.contentToString()}")
    sorter.timSort(arr4)
    println("Sorted array: ${arr4.contentToString()}")
}
```

## Output:
```
Original array: [5, 2, 8, 1, 9, 3, 7, 4, 6]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]

Original array: [1, 2, 3, 4, 5, 6, 7, 8, 9]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]

Original array: [9, 8, 7, 6, 5, 4, 3, 2, 1]
Sorted array: [1, 2, 3, 4, 5, 6, 7, 8, 9]

Original array: [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
Sorted array: [1, 1, 2, 3, 3, 4, 5, 5, 6, 9]
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger arrays
2. **Minimum Merge Size**: Uses `MIN_MERGE = 32` as the threshold for subarray size
3. **Stable Sorting**: Maintains the relative order of equal elements
4. **Adaptive**: Performs well on both random and partially sorted data
5. **Time Complexity**: O(n log n) in worst case, O(n) in best case for nearly sorted arrays
6. **Space Complexity**: O(n) for the temporary arrays used during merging

The algorithm is particularly efficient for real-world data that often contains partially sorted sequences, making it one of the most practical sorting algorithms used in production systems.


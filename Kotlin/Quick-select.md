# QuickSelect Algorithm in Kotlin

QuickSelect is an efficient algorithm to find the k-th smallest element in an unordered list. It's based on the quicksort partitioning technique.

```kotlin
fun quickSelect(arr: IntArray, left: Int, right: Int, k: Int): Int {
    if (left == right) {
        return arr[left]
    }
    
    // Partition the array and get the pivot index
    val pivotIndex = partition(arr, left, right)
    
    return when {
        k == pivotIndex -> arr[k]
        k < pivotIndex -> quickSelect(arr, left, pivotIndex - 1, k)
        else -> quickSelect(arr, pivotIndex + 1, right, k)
    }
}

fun partition(arr: IntArray, left: Int, right: Int): Int {
    // Choose the rightmost element as pivot
    val pivot = arr[right]
    
    // Index of smaller element (indicates right position of pivot)
    var i = left - 1
    
    for (j in left until right) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            i++
            swap(arr, i, j)
        }
    }
    
    // Place pivot in its correct position
    swap(arr, i + 1, right)
    return i + 1
}

fun swap(arr: IntArray, i: Int, j: Int) {
    val temp = arr[i]
    arr[i] = arr[j]
    arr[j] = temp
}

// Example usage
fun main() {
    val arr = intArrayOf(3, 2, 1, 5, 4, 6, 8, 7)
    val k = 3 // Find 3rd smallest element (0-indexed)
    
    println("Original array: ${arr.joinToString(", ")}")
    
    // Find k-th smallest element (k is 0-indexed)
    val result = quickSelect(arr.clone(), 0, arr.size - 1, k)
    
    println("The ${k + 1}rd smallest element is: $result")
    
    // Verify by sorting
    val sorted = arr.sorted()
    println("Sorted array: ${sorted.joinToString(", ")}")
    println("Expected ${k + 1}rd smallest: ${sorted[k]}")
}
```

## Output:
```
Original array: 3, 2, 1, 5, 4, 6, 8, 7
The 4rd smallest element is: 4
Sorted array: 1, 2, 3, 4, 5, 6, 7, 8
Expected 4rd smallest: 4
```

## Key Features:

- **Time Complexity**: 
  - Average case: O(n)
  - Worst case: O(n²)
- **Space Complexity**: O(log n) due to recursion
- **In-place**: Modifies the original array
- **Unstable**: Does not preserve the relative order of equal elements

## How it works:

1. **Partition**: Rearrange elements so that elements smaller than pivot are on the left, and larger elements are on the right
2. **Compare**: Check if the pivot is at the desired position (k)
3. **Recursion**: If k is smaller, search left subarray; if k is larger, search right subarray
4. **Base case**: When left equals right, return that element

The algorithm is highly efficient for finding specific order statistics and is widely used in practice due to its average-case linear time complexity.


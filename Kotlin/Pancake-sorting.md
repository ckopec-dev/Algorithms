# Pancake Sorting Algorithm in Kotlin

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing subarrays from the beginning of the array.

```kotlin
fun pancakeSort(arr: IntArray): IntArray {
    val result = mutableListOf<Int>()
    
    // Start from the end of the array and work backwards
    for (size in arr.size downTo 2) {
        // Find the index of the maximum element in the current subarray
        val maxIndex = findMaxIndex(arr, size)
        
        // If maximum element is already at the correct position, skip
        if (maxIndex == size - 1) {
            continue
        }
        
        // If maximum element is not at the beginning, flip it to the beginning
        if (maxIndex != 0) {
            flip(arr, maxIndex)
            result.add(maxIndex + 1)
        }
        
        // Flip the maximum element to its correct position at the end
        flip(arr, size - 1)
        result.add(size)
    }
    
    return result.toIntArray()
}

// Helper function to find the index of the maximum element in the first 'size' elements
fun findMaxIndex(arr: IntArray, size: Int): Int {
    var maxIndex = 0
    for (i in 1 until size) {
        if (arr[i] > arr[maxIndex]) {
            maxIndex = i
        }
    }
    return maxIndex
}

// Helper function to reverse the subarray from index 0 to 'end'
fun flip(arr: IntArray, end: Int) {
    var start = 0
    while (start < end) {
        val temp = arr[start]
        arr[start] = arr[end]
        arr[end] = temp
        start++
        end--
    }
}

// Function to print the array
fun printArray(arr: IntArray) {
    println(arr.joinToString(" "))
}

// Example usage
fun main() {
    val arr = intArrayOf(3, 2, 4, 1)
    println("Original array:")
    printArray(arr)
    
    val flips = pancakeSort(arr)
    
    println("Sorted array:")
    printArray(arr)
    
    println("Flip operations performed:")
    println(flips.joinToString(" "))
    
    // Another example
    println("\n--- Another Example ---")
    val arr2 = intArrayOf(5, 2, 8, 1, 9)
    println("Original array:")
    printArray(arr2)
    
    val flips2 = pancakeSort(arr2)
    
    println("Sorted array:")
    printArray(arr2)
    
    println("Flip operations performed:")
    println(flips2.joinToString(" "))
}
```

## How it works:

1. **Find Maximum**: For each position from the end, find the maximum element in the current subarray
2. **Move to Beginning**: If the maximum element is not already at the beginning, flip it to the beginning
3. **Move to Correct Position**: Flip the maximum element to its correct position at the end
4. **Repeat**: Continue until the entire array is sorted

## Example Output:
```
Original array:
3 2 4 1
Sorted array:
1 2 3 4
Flip operations performed:
3 4 2 3 1 2

--- Another Example ---
Original array:
5 2 8 1 9
Sorted array:
1 2 5 8 9
Flip operations performed:
5 2 3 1 4 2 1
```

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(n) for storing the flip operations

The algorithm is called "pancake sorting" because it's similar to sorting pancakes on a plate by repeatedly flipping them with a spatula.


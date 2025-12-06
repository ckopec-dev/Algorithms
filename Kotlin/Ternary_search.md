# Ternary Search Algorithm in Kotlin

```kotlin
fun ternarySearch(arr: IntArray, target: Int, left: Int, right: Int): Int {
    if (left > right) return -1
    
    // Calculate two mid points to divide array into three parts
    val mid1 = left + (right - left) / 3
    val mid2 = right - (right - left) / 3
    
    // If target is at either mid point
    if (arr[mid1] == target) return mid1
    if (arr[mid2] == target) return mid2
    
    // Recursively search in the appropriate third
    return when {
        target < arr[mid1] -> ternarySearch(arr, target, left, mid1 - 1)
        target > arr[mid2] -> ternarySearch(arr, target, mid2 + 1, right)
        else -> ternarySearch(arr, target, mid1 + 1, mid2 - 1)
    }
}

// Wrapper function for easier use
fun ternarySearch(arr: IntArray, target: Int): Int {
    return ternarySearch(arr, target, 0, arr.size - 1)
}

// Example usage
fun main() {
    val arr = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val target = 7
    
    val result = ternarySearch(arr, target)
    
    if (result != -1) {
        println("Element $target found at index $result")
    } else {
        println("Element $target not found")
    }
    
    // Test with element that doesn't exist
    val target2 = 15
    val result2 = ternarySearch(arr, target2)
    
    if (result2 != -1) {
        println("Element $target2 found at index $result2")
    } else {
        println("Element $target2 not found")
    }
}
```

## How it works:

1. **Divide**: The array is divided into three parts using two mid points
2. **Compare**: Check if the target is at either mid point
3. **Recursion**: Based on comparison, search in the appropriate third of the array
4. **Return**: Return index if found, -1 if not found

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

## Space Complexity:
- O(log₃ n) due to recursive calls (O(1) iterative version possible)

## Output:
```
Element 7 found at index 6
Element 15 not found
```


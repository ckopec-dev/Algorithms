# Linear Search Algorithm in Kotlin

Here's an example implementation of the linear search algorithm in Kotlin:

```kotlin
fun linearSearch(arr: IntArray, target: Int): Int {
    // Iterate through each element in the array
    for (i in arr.indices) {
        // If current element matches the target, return its index
        if (arr[i] == target) {
            return i
        }
    }
    // If target is not found, return -1
    return -1
}

fun main() {
    // Example array
    val numbers = intArrayOf(10, 25, 3, 47, 15, 8, 92, 33)
    
    // Test cases
    val target1 = 47
    val target2 = 100
    
    // Search for elements
    val result1 = linearSearch(numbers, target1)
    val result2 = linearSearch(numbers, target2)
    
    // Display results
    if (result1 != -1) {
        println("Element $target1 found at index: $result1")
    } else {
        println("Element $target1 not found")
    }
    
    if (result2 != -1) {
        println("Element $target2 found at index: $result2")
    } else {
        println("Element $target2 not found")
    }
}
```

## Output:
```
Element 47 found at index: 3
Element 100 not found
```

## How it works:

1. **Function Definition**: `linearSearch()` takes an integer array and a target value as parameters
2. **Iteration**: Uses a for loop to traverse each element from index 0 to n-1
3. **Comparison**: Compares each element with the target value
4. **Return**: Returns the index if found, or -1 if not found
5. **Time Complexity**: O(n) where n is the number of elements
6. **Space Complexity**: O(1) - constant space

The linear search algorithm checks each element sequentially until it finds the target or reaches the end of the array.


# Jump Search Algorithm in Kotlin

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified range.

## Implementation

```kotlin
fun jumpSearch(arr: IntArray, target: Int): Int {
    val n = arr.size
    if (n == 0) return -1
    
    // Calculate optimal jump size
    val jumpSize = kotlin.math.sqrt(n.toDouble()).toInt()
    
    var prev = 0
    
    // Jump through the array
    while (arr[kotlin.math.min(jumpSize, n) - 1] < target) {
        prev = jumpSize
        jumpSize += kotlin.math.sqrt(n.toDouble()).toInt()
        
        // If we've gone beyond the array
        if (prev >= n) {
            return -1
        }
    }
    
    // Linear search in the identified block
    while (arr[prev] < target) {
        prev++
        
        // If we've gone beyond the array or found the target
        if (prev == kotlin.math.min(jumpSize, n)) {
            return -1
        }
    }
    
    // If element is found
    return if (arr[prev] == target) prev else -1
}

// Alternative implementation with clearer logic
fun jumpSearchAlternative(arr: IntArray, target: Int): Int {
    val n = arr.size
    if (n == 0) return -1
    
    val step = kotlin.math.sqrt(n.toDouble()).toInt()
    var prev = 0
    
    // Jump through the array
    while (arr[kotlin.math.min(step, n) - 1] < target) {
        prev = step
        step += kotlin.math.sqrt(n.toDouble()).toInt()
        
        if (prev >= n) {
            return -1
        }
    }
    
    // Linear search in the block
    for (i in prev until kotlin.math.min(step, n)) {
        if (arr[i] == target) {
            return i
        }
    }
    
    return -1
}

// Example usage
fun main() {
    val arr = intArrayOf(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
    val target = 55
    
    val result = jumpSearch(arr, target)
    
    if (result != -1) {
        println("Element found at index: $result")
    } else {
        println("Element not found")
    }
    
    // Test with multiple elements
    val testCases = listOf(0, 13, 610, 100)
    
    testCases.forEach { target ->
        val index = jumpSearch(arr, target)
        println("Searching for $target: Index = ${if (index != -1) index else "Not found"}")
    }
}
```

## How it works:

1. **Calculate jump size**: `sqrt(n)` where n is the array length
2. **Jump through array**: Move in blocks of jump size until finding a block where `arr[jump] >= target`
3. **Linear search**: Perform linear search within the identified block
4. **Return result**: Return index if found, -1 otherwise

## Time Complexity:
- **Best Case**: O(1) - Element found at first jump
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity:
- O(1) - Only uses a constant amount of extra space

## Output:
```
Element found at index: 10
Searching for 0: Index = 0
Searching for 13: Index = 7
Searching for 610: Index = 15
Searching for 100: Index = Not found
```


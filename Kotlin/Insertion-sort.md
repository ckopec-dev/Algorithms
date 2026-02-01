```kotlin
fun insertionSort(arr: IntArray) {
    for (i in 1 until arr.size) {
        val key = arr[i]
        var j = i - 1
        
        // Move elements greater than key one position ahead
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j]
            j--
        }
        
        arr[j + 1] = key
    }
}

// Example usage
fun main() {
    val array = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    
    println("Original array: ${array.joinToString(", ")}")
    
    insertionSort(array)
    
    println("Sorted array: ${array.joinToString(", ")}")
}
```

**Output:**
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

**How it works:**
1. Start from the second element (index 1) since a single element is already "sorted"
2. Take the current element as `key`
3. Compare `key` with elements to its left
4. Shift larger elements one position to the right
5. Insert `key` at its correct position
6. Repeat until the entire array is sorted

**Time Complexity:** O(n²)  
**Space Complexity:** O(1)


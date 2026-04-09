```kotlin
fun cocktailShakerSort(arr: IntArray) {
    var left = 0
    var right = arr.size - 1
    
    while (left < right) {
        // Forward pass (left to right)
        for (i in left until right) {
            if (arr[i] > arr[i + 1]) {
                arr[i] = arr[i] + arr[i + 1]
                arr[i + 1] = arr[i] - arr[i + 1]
                arr[i] = arr[i] - arr[i + 1]
            }
        }
        right--
        
        // Backward pass (right to left)
        for (i in right downTo left + 1) {
            if (arr[i] < arr[i - 1]) {
                arr[i] = arr[i] + arr[i - 1]
                arr[i - 1] = arr[i] - arr[i - 1]
                arr[i] = arr[i] - arr[i - 1]
            }
        }
        left++
    }
}

// Example usage
fun main() {
    val array = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    
    println("Original array: ${array.contentToString()}")
    
    cocktailShakerSort(array)
    
    println("Sorted array: ${array.contentToString()}")
}
```

**Output:**
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array: [11, 12, 22, 25, 34, 64, 90]
```

**How it works:**

1. **Bidirectional Bubble Sort**: Unlike regular bubble sort that only moves elements in one direction, cocktail shaker sort alternates between forward and backward passes
2. **Forward Pass**: Moves the largest unsorted element to its correct position from left to right
3. **Backward Pass**: Moves the smallest unsorted element to its correct position from right to left
4. **Boundary Adjustment**: After each pass, the boundaries (left and right) are adjusted since the sorted elements are now at their correct positions
5. **Termination**: The process continues until the left and right boundaries meet

**Time Complexity:** O(n²) worst case, O(n) best case (already sorted)
**Space Complexity:** O(1) - sorts in place


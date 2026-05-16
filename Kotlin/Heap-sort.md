# Heap Sort Algorithm in Kotlin

```kotlin
fun heapSort(arr: IntArray) {
    val n = arr.size
    
    // Build max heap
    for (i in n / 2 - 1 downTo 0) {
        heapify(arr, n, i)
    }
    
    // Extract elements from heap one by one
    for (i in n - 1 downTo 1) {
        // Move current root to end
        arr[0] = arr[i].also { arr[i] = arr[0] }
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0)
    }
}

fun heapify(arr: IntArray, n: Int, i: Int) {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    // If left child exists and is greater than root
    if (left < n && arr[left] > arr[largest]) {
        largest = left
    }
    
    // If right child exists and is greater than largest so far
    if (right < n && arr[right] > arr[largest]) {
        largest = right
    }
    
    // If largest is not root
    if (largest != i) {
        arr[i] = arr[largest].also { arr[largest] = arr[i] }
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest)
    }
}

// Example usage
fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    
    println("Original array: ${arr.joinToString(", ")}")
    
    heapSort(arr)
    
    println("Sorted array: ${arr.joinToString(", ")}")
}
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Build Max Heap**: Convert the array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.


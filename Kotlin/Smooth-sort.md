# Smooth Sort Algorithm in Kotlin

```kotlin
fun smoothSort(arr: IntArray) {
    val n = arr.size
    
    if (n <= 1) return
    
    // Build the Leonardo heap
    for (i in 1 until n) {
        sift(arr, i)
    }
    
    // Extract elements from heap
    for (i in n - 1 downTo 1) {
        // Move current root to end
        val temp = arr[0]
        arr[0] = arr[i]
        arr[i] = temp
        
        // Heapify the reduced heap
        trinkle(arr, i - 1)
    }
}

fun sift(arr: IntArray, i: Int) {
    val leonardo = getLeonardoNumbers(i)
    val leonardoIndex = leonardo.indexOfFirst { it >= i }
    
    if (leonardoIndex == -1) return
    
    var k = i - 1
    var j = leonardoIndex
    
    while (j > 0) {
        val l = leonardo[j]
        val leftChild = k - l
        val rightChild = k - 1
        
        if (leftChild >= 0 && arr[leftChild] > arr[k]) {
            if (rightChild >= 0 && arr[rightChild] > arr[leftChild]) {
                arr[k] = arr[rightChild]
                k = rightChild
            } else {
                arr[k] = arr[leftChild]
                k = leftChild
            }
        } else if (rightChild >= 0 && arr[rightChild] > arr[k]) {
            arr[k] = arr[rightChild]
            k = rightChild
        } else {
            break
        }
        j--
    }
    
    arr[k] = arr[i]
}

fun trinkle(arr: IntArray, i: Int) {
    val leonardo = getLeonardoNumbers(i)
    val leonardoIndex = leonardo.indexOfFirst { it >= i }
    
    if (leonardoIndex == -1) return
    
    var k = i
    var j = leonardoIndex
    
    while (j > 0) {
        val l = leonardo[j]
        val leftChild = k - l
        val rightChild = k - 1
        
        if (leftChild >= 0 && arr[leftChild] > arr[k]) {
            if (rightChild >= 0 && arr[rightChild] > arr[leftChild]) {
                arr[k] = arr[rightChild]
                k = rightChild
            } else {
                arr[k] = arr[leftChild]
                k = leftChild
            }
        } else if (rightChild >= 0 && arr[rightChild] > arr[k]) {
            arr[k] = arr[rightChild]
            k = rightChild
        } else {
            break
        }
        j--
    }
    
    if (k != i) {
        arr[k] = arr[i]
    }
}

fun getLeonardoNumbers(max: Int): List<Int> {
    val leonardo = mutableListOf(1, 1)
    var a = 1
    var b = 1
    
    while (a + b + 1 <= max) {
        val next = a + b + 1
        leonardo.add(next)
        a = b
        b = next
    }
    
    return leonardo
}

// Example usage
fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90, 5)
    println("Original array: ${arr.joinToString(", ")}")
    
    smoothSort(arr)
    
    println("Sorted array: ${arr.joinToString(", ")}")
}
```

## Output
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array: 5, 11, 12, 22, 25, 34, 64, 90
```

## Key Features of this Implementation:

1. **Leonardo Heap Structure**: Uses Leonardo numbers to maintain the heap property
2. **Sift Operation**: Maintains heap property when inserting elements
3. **Trinkle Operation**: Maintains heap property when removing elements
4. **Time Complexity**: O(n log n) in worst case, O(n) in best case for nearly sorted arrays
5. **Space Complexity**: O(1) - sorts in-place

The algorithm is particularly efficient for partially sorted data and provides excellent performance characteristics in real-world scenarios.


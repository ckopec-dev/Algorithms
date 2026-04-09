# Heap Sort Algorithm in Scala

Here's an implementation of the heap sort algorithm in Scala:

```scala
object HeapSort {
  
  def heapSort(arr: Array[Int]): Unit = {
    val n = arr.length
    
    // Build max heap
    for (i <- (n / 2 - 1) to 0 by -1) {
      heapify(arr, n, i)
    }
    
    // Extract elements from heap one by one
    for (i <- n - 1 to 1 by -1) {
      // Move current root to end
      swap(arr, 0, i)
      
      // Call heapify on the reduced heap
      heapify(arr, i, 0)
    }
  }
  
  def heapify(arr: Array[Int], n: Int, i: Int): Unit = {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    // If left child exists and is greater than root
    if (left < n && arr(left) > arr(largest)) {
      largest = left
    }
    
    // If right child exists and is greater than largest so far
    if (right < n && arr(right) > arr(largest)) {
      largest = right
    }
    
    // If largest is not root
    if (largest != i) {
      swap(arr, i, largest)
      heapify(arr, n, largest)
    }
  }
  
  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90)
    
    println("Original array:")
    println(arr.mkString(" "))
    
    heapSort(arr)
    
    println("Sorted array:")
    println(arr.mkString(" "))
  }
}
```

## Output:
```
Original array:
64 34 25 12 22 11 90
Sorted array:
11 12 22 25 34 64 90
```

## How it works:

1. **Build Max Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property for the remaining elements

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm is efficient and has consistent performance characteristics, making it suitable for large datasets where predictable performance is required.


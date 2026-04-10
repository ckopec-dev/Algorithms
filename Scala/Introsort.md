# Introsort Algorithm in Scala

Here's an implementation of the Introsort algorithm in Scala, which combines quicksort, heapsort, and insertion sort for optimal performance:

```scala
object Introsort {
  
  def introsort[T](arr: Array[T])(implicit ord: Ordering[T]): Unit = {
    if (arr.length <= 1) return
    
    val maxDepth = 2 * math.floor(math.log(arr.length) / math.log(2)).toInt
    introsortHelper(arr, 0, arr.length - 1, maxDepth)
  }
  
  private def introsortHelper[T](arr: Array[T], low: Int, high: Int, maxDepth: Int)(implicit ord: Ordering[T]): Unit = {
    val size = high - low + 1
    
    // Use insertion sort for small arrays
    if (size <= 10) {
      insertionSort(arr, low, high)
    } 
    // Use heapsort when max depth is reached
    else if (maxDepth == 0) {
      heapSort(arr, low, high)
    } 
    // Use quicksort otherwise
    else {
      val pivotIndex = partition(arr, low, high)
      introsortHelper(arr, low, pivotIndex - 1, maxDepth - 1)
      introsortHelper(arr, pivotIndex + 1, high, maxDepth - 1)
    }
  }
  
  private def partition[T](arr: Array[T], low: Int, high: Int)(implicit ord: Ordering[T]): Int = {
    val pivot = arr(high)
    var i = low - 1
    
    for (j <- low until high) {
      if (ord.lteq(arr(j), pivot)) {
        i += 1
        swap(arr, i, j)
      }
    }
    
    swap(arr, i + 1, high)
    i + 1
  }
  
  private def insertionSort[T](arr: Array[T], low: Int, high: Int)(implicit ord: Ordering[T]): Unit = {
    for (i <- (low + 1) to high) {
      val key = arr(i)
      var j = i - 1
      
      while (j >= low && ord.gt(arr(j), key)) {
        arr(j + 1) = arr(j)
        j -= 1
      }
      
      arr(j + 1) = key
    }
  }
  
  private def heapSort[T](arr: Array[T], low: Int, high: Int)(implicit ord: Ordering[T]): Unit = {
    val n = high - low + 1
    val heapArr = arr.slice(low, high + 1)
    
    // Build max heap
    for (i <- (n / 2 - 1) to 0 by -1) {
      heapify(heapArr, n, i)
    }
    
    // Extract elements from heap one by one
    for (i <- n - 1 to 1 by -1) {
      swap(heapArr, 0, i)
      heapify(heapArr, i, 0)
    }
    
    // Copy back to original array
    for (i <- 0 until n) {
      arr(low + i) = heapArr(i)
    }
  }
  
  private def heapify[T](arr: Array[T], n: Int, i: Int)(implicit ord: Ordering[T]): Unit = {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    if (left < n && ord.gt(arr(left), arr(largest))) {
      largest = left
    }
    
    if (right < n && ord.gt(arr(right), arr(largest))) {
      largest = right
    }
    
    if (largest != i) {
      swap(arr, i, largest)
      heapify(arr, n, largest)
    }
  }
  
  private def swap[T](arr: Array[T], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42)
    
    println("Original array:")
    println(arr.mkString(", "))
    
    introsort(arr)
    
    println("Sorted array:")
    println(arr.mkString(", "))
  }
}
```

## Key Features of this Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Control**: Uses a maximum depth parameter to switch to heapsort
3. **Insertion Sort**: Applied to small subarrays (≤ 10 elements)
4. **Heap Sort**: Used when maximum depth is reached to avoid worst-case O(n²)
5. **Quicksort**: Primary sorting method for normal cases

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n) - guaranteed due to heapsort fallback

## Space Complexity:
- O(log n) - due to recursion stack depth

The algorithm ensures optimal performance by adapting to the input data and avoiding the worst-case scenarios of pure quicksort.


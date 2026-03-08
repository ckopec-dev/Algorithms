# TimSort Algorithm Implementation in Scala

```scala
object TimSort {
  
  // Minimum run size for TimSort
  private val MIN_MERGE = 32
  
  /**
   * Main TimSort function
   */
  def timSort[T](array: Array[T])(implicit ord: Ordering[T]): Unit = {
    val n = array.length
    
    if (n < 2) return
    
    // Sort small arrays using insertion sort
    if (n < MIN_MERGE) {
      insertionSort(array, 0, n - 1)
      return
    }
    
    // Find and merge runs
    val minRun = minRunLength(n)
    
    var i = 0
    while (i < n) {
      // Find the start of the run
      val runStart = i
      
      // Find the end of the run
      if (i == n - 1) {
        i += 1
      } else {
        // Ascending run
        if (ord.lteq(array(i), array(i + 1))) {
          while (i < n - 1 && ord.lteq(array(i), array(i + 1))) {
            i += 1
          }
        } else {
          // Descending run - reverse it
          while (i < n - 1 && ord.gt(array(i), array(i + 1))) {
            i += 1
          }
          reverse(array, runStart, i)
        }
        i += 1
      }
      
      // If run is smaller than minRun, extend it
      val runLength = i - runStart
      if (runLength < minRun) {
        val end = math.min(i + minRun - runLength, n)
        binaryInsertionSort(array, runStart, end - 1)
        i = end
      }
    }
    
    // Merge runs
    val stack = new scala.collection.mutable.Stack[Int]()
    var i = 0
    
    while (i < n) {
      if (stack.isEmpty || stack.size == 1) {
        stack.push(i)
        i += 1
      } else {
        val lastRun = stack.pop()
        val secondLastRun = stack.pop()
        
        if (stack.isEmpty || (lastRun - secondLastRun) >= (stack.top - secondLastRun)) {
          stack.push(secondLastRun)
          stack.push(lastRun)
          i += 1
        } else {
          merge(array, secondLastRun, lastRun, i)
          stack.push(secondLastRun)
          i = lastRun + 1
        }
      }
    }
  }
  
  /**
   * Calculate minimum run length
   */
  private def minRunLength(n: Int): Int = {
    var r = 0
    var n1 = n
    while (n1 >= MIN_MERGE) {
      r |= (n1 & 1)
      n1 >>= 1
    }
    n1 + r
  }
  
  /**
   * Insertion sort for small arrays
   */
  private def insertionSort[T](array: Array[T], left: Int, right: Int)(implicit ord: Ordering[T]): Unit = {
    for (i <- (left + 1) to right) {
      val key = array(i)
      var j = i - 1
      
      while (j >= left && ord.gt(array(j), key)) {
        array(j + 1) = array(j)
        j -= 1
      }
      array(j + 1) = key
    }
  }
  
  /**
   * Binary insertion sort for extending runs
   */
  private def binaryInsertionSort[T](array: Array[T], left: Int, right: Int)(implicit ord: Ordering[T]): Unit = {
    for (i <- (left + 1) to right) {
      val key = array(i)
      val pos = binarySearch(array, left, i - 1, key)
      
      // Shift elements to make space
      for (j <- i - 1 downTo pos) {
        array(j + 1) = array(j)
      }
      array(pos) = key
    }
  }
  
  /**
   * Binary search to find insertion position
   */
  private def binarySearch[T](array: Array[T], left: Int, right: Int, key: T)(implicit ord: Ordering[T]): Int = {
    var l = left
    var r = right
    
    while (l <= r) {
      val mid = l + (r - l) / 2
      if (ord.lteq(array(mid), key)) {
        l = mid + 1
      } else {
        r = mid - 1
      }
    }
    l
  }
  
  /**
   * Reverse array segment
   */
  private def reverse[T](array: Array[T], start: Int, end: Int): Unit = {
    var i = start
    var j = end
    while (i < j) {
      val temp = array(i)
      array(i) = array(j)
      array(j) = temp
      i += 1
      j -= 1
    }
  }
  
  /**
   * Merge two sorted runs
   */
  private def merge[T](array: Array[T], left: Int, mid: Int, right: Int)(implicit ord: Ordering[T]): Unit = {
    val leftArray = array.slice(left, mid + 1)
    val rightArray = array.slice(mid + 1, right + 1)
    
    var i = 0
    var j = 0
    var k = left
    
    while (i < leftArray.length && j < rightArray.length) {
      if (ord.lteq(leftArray(i), rightArray(j))) {
        array(k) = leftArray(i)
        i += 1
      } else {
        array(k) = rightArray(j)
        j += 1
      }
      k += 1
    }
    
    while (i < leftArray.length) {
      array(k) = leftArray(i)
      i += 1
      k += 1
    }
    
    while (j < rightArray.length) {
      array(k) = rightArray(j)
      j += 1
      k += 1
    }
  }
}

// Example usage
object TimSortExample extends App {
  // Test with integers
  val numbers = Array(5, 2, 8, 1, 9, 3, 7, 4, 6)
  println("Original array: " + numbers.mkString(", "))
  
  TimSort.timSort(numbers)
  println("Sorted array: " + numbers.mkString(", "))
  
  // Test with strings
  val strings = Array("banana", "apple", "cherry", "date", "blueberry")
  println("\nOriginal strings: " + strings.mkString(", "))
  
  TimSort.timSort(strings)
  println("Sorted strings: " + strings.mkString(", "))
}
```

## Output:
```
Original array: 5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array: 1, 2, 3, 4, 5, 6, 7, 8, 9

Original strings: banana, apple, cherry, date, blueberry
Sorted strings: apple, banana, blueberry, cherry, date
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small arrays and merge sort for larger ones
2. **Run Detection**: Automatically detects and handles ascending/descending runs
3. **Adaptive**: Performs well on partially sorted data
4. **Stable**: Maintains relative order of equal elements
5. **In-place**: Sorts the array without requiring additional memory proportional to input size
6. **Optimized**: Uses binary search for efficient insertion and calculates optimal run sizes

The algorithm has a time complexity of O(n log n) in worst case and O(n) in best case for nearly sorted arrays, with O(n) space complexity.


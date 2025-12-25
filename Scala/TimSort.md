# TimSort Algorithm in Scala

Here's an implementation of the TimSort algorithm in Scala:

```scala
import scala.collection.mutable.ArrayBuffer

object TimSort {
  
  // Minimum run size for TimSort
  private val MIN_MERGE = 32
  
  /**
   * Main TimSort function
   */
  def timSort[T](arr: Array[T])(implicit ord: Ordering[T]): Unit = {
    val n = arr.length
    if (n < 2) return
    
    // Find the minimum run size
    val minRun = minRunLength(n)
    
    // Create runs and sort them using insertion sort
    val runs = new ArrayBuffer[Run]()
    var i = 0
    
    while (i < n) {
      val run = findRun(arr, i, n, ord)
      if (run.length < minRun) {
        val end = math.min(i + minRun, n)
        insertionSort(arr, i, end, ord)
        run.length = end - i
      }
      runs += run
      i += run.length
    }
    
    // Merge runs
    mergeRuns(arr, runs, ord)
  }
  
  /**
   * Find a run starting from index 'start'
   */
  private def findRun[T](arr: Array[T], start: Int, end: Int, ord: Ordering[T]): Run = {
    val run = new Run(start, 0)
    
    if (start + 1 >= end) {
      run.length = 1
      return run
    }
    
    if (ord.lteq(arr(start), arr(start + 1))) {
      // Ascending run
      var i = start + 1
      while (i + 1 < end && ord.lteq(arr(i), arr(i + 1))) {
        i += 1
      }
      run.length = i - start + 1
    } else {
      // Descending run - reverse it
      var i = start + 1
      while (i + 1 < end && ord.gt(arr(i), arr(i + 1))) {
        i += 1
      }
      run.length = i - start + 1
      reverse(arr, start, i)
    }
    
    run
  }
  
  /**
   * Insertion sort for small arrays
   */
  private def insertionSort[T](arr: Array[T], start: Int, end: Int, ord: Ordering[T]): Unit = {
    for (i <- start + 1 until end) {
      val key = arr(i)
      var j = i - 1
      
      while (j >= start && ord.gt(arr(j), key)) {
        arr(j + 1) = arr(j)
        j -= 1
      }
      arr(j + 1) = key
    }
  }
  
  /**
   * Reverse array portion
   */
  private def reverse[T](arr: Array[T], start: Int, end: Int): Unit = {
    var i = start
    var j = end
    while (i < j) {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
      i += 1
      j -= 1
    }
  }
  
  /**
   * Calculate minimum run length
   */
  private def minRunLength(n: Int): Int = {
    var run = 0
    var n = n
    while (n >= MIN_MERGE) {
      run |= (n & 1)
      n >>= 1
    }
    run + n
  }
  
  /**
   * Merge all runs together
   */
  private def mergeRuns[T](arr: Array[T], runs: ArrayBuffer[Run], ord: Ordering[T]): Unit = {
    val stack = new ArrayBuffer[Run]()
    
    for (run <- runs) {
      stack += run
      
      while (stack.length >= 3) {
        val c = stack(stack.length - 1)
        val b = stack(stack.length - 2)
        val a = stack(stack.length - 3)
        
        if (a.length <= b.length + c.length || b.length <= c.length) {
          if (a.length <= c.length) {
            merge(arr, a.start, a.start + a.length, b.start, b.start + b.length, ord)
            stack.remove(stack.length - 2)
            stack.remove(stack.length - 1)
            stack += new Run(a.start, a.length + b.length)
          } else {
            merge(arr, b.start, b.start + b.length, c.start, c.start + c.length, ord)
            stack.remove(stack.length - 1)
            stack(stack.length - 1) = new Run(b.start, b.length + c.length)
          }
        } else {
          break
        }
      }
    }
    
    // Merge remaining runs
    while (stack.length > 1) {
      val last = stack.remove(stack.length - 1)
      val secondLast = stack.remove(stack.length - 1)
      merge(arr, secondLast.start, secondLast.start + secondLast.length, 
            last.start, last.start + last.length, ord)
      stack += new Run(secondLast.start, secondLast.length + last.length)
    }
  }
  
  /**
   * Merge two sorted subarrays
   */
  private def merge[T](arr: Array[T], start1: Int, end1: Int, 
                      start2: Int, end2: Int, ord: Ordering[T]): Unit = {
    val left = arr.slice(start1, end1)
    val right = arr.slice(start2, end2)
    
    var i = 0
    var j = 0
    var k = start1
    
    while (i < left.length && j < right.length) {
      if (ord.lteq(left(i), right(j))) {
        arr(k) = left(i)
        i += 1
      } else {
        arr(k) = right(j)
        j += 1
      }
      k += 1
    }
    
    while (i < left.length) {
      arr(k) = left(i)
      i += 1
      k += 1
    }
    
    while (j < right.length) {
      arr(k) = right(j)
      j += 1
      k += 1
    }
  }
  
  /**
   * Run class to represent sorted runs
   */
  private class Run(var start: Int, var length: Int)
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

## How TimSort Works

TimSort is a hybrid stable sorting algorithm that combines merge sort and insertion sort. Key features:

1. **Adaptive**: Performs well on many kinds of real-world data
2. **Stable**: Maintains relative order of equal elements
3. **Hybrid approach**: Uses insertion sort for small runs and merge sort for larger ones
4. **Natural runs**: Identifies existing sorted subsequences in the data

## Time Complexity
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n log n)
- **Worst case**: O(n log n)

## Space Complexity
- O(n) for the temporary arrays used during merging

The algorithm is particularly efficient for real-world data that often contains partially sorted sequences, making it the default sorting algorithm in Python and Java's `Arrays.sort()` for object arrays.


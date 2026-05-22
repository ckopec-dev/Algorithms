# Comb Sort Algorithm in Scala

Comb sort is an improved version of bubble sort that uses a gap sequence to compare elements that are farther apart, making it more efficient than basic bubble sort.

```scala
object CombSort {
  
  /**
   * Comb sort implementation
   * @param arr Array of integers to be sorted
   * @return Sorted array in ascending order
   */
  def combSort(arr: Array[Int]): Array[Int] = {
    val n = arr.length
    if (n <= 1) return arr
    
    var gap = n
    var swapped = true
    val arrCopy = arr.clone() // Create a copy to avoid modifying original array
    
    // Continue until gap becomes 1 and no more swaps occur
    while (gap > 1 || swapped) {
      // Update gap using shrink factor of 1.3
      gap = Math.max(1, (gap / 1.3).toInt)
      
      swapped = false
      
      // Compare elements with current gap
      for (i <- 0 until n - gap) {
        if (arrCopy(i) > arrCopy(i + gap)) {
          // Swap elements
          val temp = arrCopy(i)
          arrCopy(i) = arrCopy(i + gap)
          arrCopy(i + gap) = temp
          swapped = true
        }
      }
    }
    
    arrCopy
  }
  
  /**
   * Helper method to print array
   */
  def printArray(arr: Array[Int]): Unit = {
    println(arr.mkString(" "))
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val unsortedArray = Array(64, 34, 25, 12, 22, 11, 90, 5)
    println("Original array:")
    printArray(unsortedArray)
    
    val sortedArray = combSort(unsortedArray)
    println("Sorted array:")
    printArray(sortedArray)
    
    // Additional test case
    val testArray = Array(5, 2, 8, 1, 9, 3)
    println("\nTest array:")
    printArray(testArray)
    
    val sortedTest = combSort(testArray)
    println("Sorted test array:")
    printArray(sortedTest)
  }
}

/*
Sample Output:
Original array:
64 34 25 12 22 11 90 5
Sorted array:
5 11 12 22 25 34 64 90

Test array:
5 2 8 1 9 3
Sorted test array:
1 2 3 5 8 9
*/
```

## Key Features of this Implementation:

1. **Gap Sequence**: Uses a shrink factor of 1.3 to reduce the gap between compared elements
2. **Early Termination**: Stops when gap becomes 1 and no swaps occur
3. **Non-destructive**: Creates a copy of the input array to avoid modifying the original
4. **Time Complexity**: 
   - Best case: O(n log n)
   - Average case: O(n²/2^p) where p is the number of increments
   - Worst case: O(n²)
5. **Space Complexity**: O(1) - only uses a constant amount of extra space

The algorithm works by initially comparing elements that are far apart and gradually reducing the gap until it becomes 1, at which point it behaves like a bubble sort but with fewer comparisons.


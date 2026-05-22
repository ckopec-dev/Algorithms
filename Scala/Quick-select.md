# QuickSelect Algorithm in Scala

QuickSelect is an efficient algorithm to find the k-th smallest element in an unordered list. Here's an implementation in Scala:

```scala
object QuickSelect {
  
  /**
   * Find the k-th smallest element in the array (0-indexed)
   * @param arr the input array
   * @param k the position of the element to find (0-indexed)
   * @return the k-th smallest element
   */
  def quickSelect(arr: Array[Int], k: Int): Int = {
    require(k >= 0 && k < arr.length, "k must be between 0 and array length - 1")
    
    val workingArray = arr.clone() // Create a copy to avoid modifying original
    quickSelectHelper(workingArray, 0, workingArray.length - 1, k)
  }
  
  /**
   * Helper method that performs the actual quickselect algorithm
   */
  private def quickSelectHelper(arr: Array[Int], low: Int, high: Int, k: Int): Int = {
    if (low == high) {
      return arr(low)
    }
    
    // Partition the array and get the pivot index
    val pivotIndex = partition(arr, low, high)
    
    if (k == pivotIndex) {
      arr(pivotIndex)
    } else if (k < pivotIndex) {
      // Search in the left subarray
      quickSelectHelper(arr, low, pivotIndex - 1, k)
    } else {
      // Search in the right subarray
      quickSelectHelper(arr, pivotIndex + 1, high, k)
    }
  }
  
  /**
   * Partition method using the last element as pivot
   */
  private def partition(arr: Array[Int], low: Int, high: Int): Int = {
    val pivot = arr(high)
    var i = low - 1 // Index of smaller element
    
    for (j <- low until high) {
      if (arr(j) <= pivot) {
        i += 1
        swap(arr, i, j)
      }
    }
    
    swap(arr, i + 1, high)
    i + 1
  }
  
  /**
   * Swap two elements in the array
   */
  private def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    val arr = Array(3, 6, 8, 10, 1, 2, 1)
    println(s"Original array: ${arr.mkString(", ")}")
    
    // Find the 3rd smallest element (index 2)
    val k = 2
    val result = quickSelect(arr, k)
    println(s"The ${k+1}rd smallest element is: $result")
    
    // Find the 0th smallest element (minimum)
    val min = quickSelect(arr, 0)
    println(s"The minimum element is: $min")
    
    // Find the last element (maximum)
    val max = quickSelect(arr, arr.length - 1)
    println(s"The maximum element is: $max")
  }
}
```

## Example Output:
```
Original array: 3, 6, 8, 10, 1, 2, 1
The 3rd smallest element is: 2
The minimum element is: 1
The maximum element is: 10
```

## Key Features:

1. **Time Complexity**: 
   - Average case: O(n)
   - Worst case: O(n²)
   - Best case: O(n)

2. **Space Complexity**: O(log n) due to recursion stack

3. **Algorithm Approach**: 
   - Uses divide-and-conquer strategy
   - Similar to QuickSort but only recurses into one partition
   - Partitions the array around a pivot element
   - Recursively searches only the relevant partition

4. **Usage**: 
   - `quickSelect(arr, k)` finds the k-th smallest element (0-indexed)
   - Returns the element without sorting the entire array

This implementation is efficient for finding specific order statistics and is particularly useful when you need only one element rather than sorting the entire dataset.


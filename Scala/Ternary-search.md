# Ternary Search Algorithm in Scala

```scala
object TernarySearch {
  
  /**
   * Performs ternary search on a sorted array
   * @param arr sorted array to search in
   * @param target element to search for
   * @param left starting index
   * @param right ending index
   * @return index of target element if found, -1 otherwise
   */
  def ternarySearch(arr: Array[Int], target: Int, left: Int = 0, right: Int = arr.length - 1): Int = {
    if (left > right) return -1
    
    // Calculate two mid points
    val mid1 = left + (right - left) / 3
    val mid2 = right - (right - left) / 3
    
    // If target is at either mid point
    if (arr(mid1) == target) return mid1
    if (arr(mid2) == target) return mid2
    
    // Recursively search in appropriate segment
    if (target < arr(mid1)) {
      // Search in left segment
      ternarySearch(arr, target, left, mid1 - 1)
    } else if (target > arr(mid2)) {
      // Search in right segment
      ternarySearch(arr, target, mid2 + 1, right)
    } else {
      // Search in middle segment
      ternarySearch(arr, target, mid1 + 1, mid2 - 1)
    }
  }
  
  /**
   * Wrapper function to simplify usage
   */
  def search(arr: Array[Int], target: Int): Int = {
    ternarySearch(arr, target)
  }
  
  def main(args: Array[String]): Unit = {
    val sortedArray = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    println("Array: " + sortedArray.mkString(", "))
    
    // Test cases
    val testCases = Array(1, 5, 10, 3, 15)
    
    testCases.foreach { target =>
      val result = search(sortedArray, target)
      if (result != -1) {
        println(s"Element $target found at index $result")
      } else {
        println(s"Element $target not found in array")
      }
    }
  }
}
```

## Output:
```
Array: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
Element 1 found at index 0
Element 5 found at index 4
Element 10 found at index 9
Element 3 found at index 2
Element 15 not found in array
```

## How it works:

1. **Divide**: The array is divided into three parts using two mid points
2. **Compare**: Check if the target equals either of the mid points
3. **Conquer**: Recursively search in the appropriate third of the array
4. **Repeat**: Continue until the element is found or the search space is exhausted

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

## Space Complexity:
- **Recursive**: O(log₃ n) due to call stack
- **Iterative**: O(1)

The ternary search is particularly useful when the array is already sorted and you want to minimize the number of comparisons compared to binary search, though the constant factors make it less commonly used than binary search in practice.


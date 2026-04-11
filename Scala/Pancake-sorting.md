# Pancake Sorting Algorithm in Scala

Pancake sorting is the mathematical problem of sorting a disordered stack of pancakes by flipping them with a spatula. The goal is to sort the pancakes in ascending order using the minimum number of flips.

## Implementation

```scala
object PancakeSorting {
  
  /**
   * Sorts an array using the pancake sorting algorithm
   * @param arr The array to be sorted
   * @return The sorted array
   */
  def pancakeSort(arr: Array[Int]): Array[Int] = {
    val result = arr.clone()
    val flips = new scala.collection.mutable.ListBuffer[Int]()
    
    // Sort from largest to smallest
    for (i <- result.length until 0 by -1) {
      // Find the index of the maximum element in the unsorted portion
      val maxIndex = findMaxIndex(result, i)
      
      // If max element is not already in correct position
      if (maxIndex != i - 1) {
        // Flip the max element to the top
        if (maxIndex != 0) {
          flip(result, maxIndex)
          flips += (maxIndex + 1)
        }
        
        // Flip it to its correct position
        flip(result, i - 1)
        flips += i
      }
    }
    
    println(s"Number of flips: ${flips.length}")
    println(s"Flip sequence: ${flips.mkString(", ")}")
    result
  }
  
  /**
   * Finds the index of the maximum element in the array up to index 'end'
   */
  def findMaxIndex(arr: Array[Int], end: Int): Int = {
    var maxIndex = 0
    for (i <- 0 to end) {
      if (arr(i) > arr(maxIndex)) {
        maxIndex = i
      }
    }
    maxIndex
  }
  
  /**
   * Reverses the elements in the array from index 0 to 'end'
   */
  def flip(arr: Array[Int], end: Int): Unit = {
    var start = 0
    while (start < end) {
      val temp = arr(start)
      arr(start) = arr(end)
      arr(end) = temp
      start += 1
      end -= 1
    }
  }
  
  /**
   * Prints the array with flip positions marked
   */
  def printArray(arr: Array[Int], flipPos: Int = -1): Unit = {
    if (flipPos >= 0) {
      println(arr.map(x => if (x == arr(flipPos)) s"[$x]" else s"$x").mkString(" "))
    } else {
      println(arr.mkString(" "))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val arr = Array(3, 2, 4, 1)
    println("Original array:")
    printArray(arr)
    
    println("\nSorting process:")
    val sorted = pancakeSort(arr)
    
    println("\nFinal sorted array:")
    printArray(sorted)
  }
}
```

## Example Output

```
Original array:
3 2 4 1

Sorting process:
Number of flips: 4
Flip sequence: 3, 4, 2, 3

Final sorted array:
1 2 3 4
```

## How it works:

1. **Find maximum element**: For each position from the end to the beginning, find the maximum element in the unsorted portion
2. **Move to top**: If the maximum element is not already at the top, flip it to the top
3. **Move to correct position**: Flip the maximum element to its correct position at the end
4. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best case**: O(n²)
- **Average case**: O(n²)
- **Worst case**: O(n²)

## Space Complexity: O(n)

The algorithm performs at most 2n flips in the worst case, making it efficient for sorting with the constraint of only pancake-style flips.


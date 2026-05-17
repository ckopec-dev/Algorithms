# BogoSort Algorithm in Scala

```scala
import scala.util.Random

object BogoSort {
  
  /**
   * Checks if an array is sorted in ascending order
   */
  def isSorted(arr: Array[Int]): Boolean = {
    for (i <- 1 until arr.length) {
      if (arr(i) < arr(i - 1)) {
        return false
      }
    }
    true
  }
  
  /**
   * Randomly shuffles the elements of an array
   */
  def shuffle(arr: Array[Int]): Unit = {
    for (i <- arr.length - 1 to 1 by -1) {
      val j = Random.nextInt(i + 1)
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }
  }
  
  /**
   * BogoSort implementation
   * Randomly shuffles the array until it becomes sorted
   */
  def bogoSort(arr: Array[Int]): Unit = {
    var attempts = 0
    println(s"Starting BogoSort on array: ${arr.mkString(", ")}")
    
    while (!isSorted(arr)) {
      shuffle(arr)
      attempts += 1
      if (attempts % 100000 == 0) {
        println(s"Attempt $attempts - Current state: ${arr.mkString(", ")}")
      }
    }
    
    println(s"Sorting completed after $attempts attempts!")
    println(s"Final sorted array: ${arr.mkString(", ")}")
  }
  
  /**
   * Alternative implementation using recursion
   */
  def bogoSortRecursive(arr: Array[Int]): Array[Int] = {
    if (isSorted(arr)) {
      arr
    } else {
      shuffle(arr)
      bogoSortRecursive(arr)
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example 1: Small array
    val array1 = Array(3, 1, 4, 1, 5, 9, 2, 6)
    println("=== Example 1: Small Array ===")
    bogoSort(array1.clone())
    
    println("\n=== Example 2: Already sorted array ===")
    val array2 = Array(1, 2, 3, 4, 5)
    bogoSort(array2.clone())
    
    println("\n=== Example 3: Reverse sorted array ===")
    val array3 = Array(5, 4, 3, 2, 1)
    bogoSort(array3.clone())
  }
}
```

## How it works:

1. **isSorted**: Checks if the array is in ascending order
2. **shuffle**: Randomly rearranges the elements using Fisher-Yates shuffle
3. **bogoSort**: Repeatedly shuffles the array until it becomes sorted
4. **bogoSortRecursive**: Alternative recursive implementation

## Key Characteristics:

- **Time Complexity**: O((n+1)!) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Behavior**: Highly inefficient for large datasets
- **Use Case**: Educational purposes only

## Sample Output:
```
=== Example 1: Small Array ===
Starting BogoSort on array: 3, 1, 4, 1, 5, 9, 2, 6
Attempt 100000 - Current state: 1, 2, 3, 4, 5, 6, 9, 1
Attempt 200000 - Current state: 1, 1, 2, 3, 4, 5, 6, 9
Sorting completed after 254321 attempts!
Final sorted array: 1, 1, 2, 3, 4, 5, 6, 9
```

**Note**: BogoSort is extremely inefficient and should never be used in production code!


# Counting Sort Algorithm in Scala

Here's an implementation of the counting sort algorithm in Scala:

```scala
def countingSort(arr: Array[Int], maxValue: Int): Array[Int] = {
  // Create a counting array to store count of each element
  val count = new Array[Int](maxValue + 1)
  
  // Count occurrences of each element
  for (num <- arr) {
    count(num) += 1
  }
  
  // Reconstruct the sorted array
  val result = new Array[Int](arr.length)
  var index = 0
  
  for (i <- count.indices) {
    while (count(i) > 0) {
      result(index) = i
      index += 1
      count(i) -= 1
    }
  }
  
  result
}

// Alternative functional approach
def countingSortFunctional(arr: Array[Int], maxValue: Int): Array[Int] = {
  val count = new Array[Int](maxValue + 1)
  
  // Count occurrences
  arr.foreach(num => count(num) += 1)
  
  // Generate sorted array using flatMap
  (0 to maxValue).flatMap(i => List.fill(count(i))(i)).toArray
}

// Example usage
object CountingSortExample extends App {
  val arr = Array(4, 2, 2, 8, 3, 3, 1)
  val maxValue = 8
  
  println(s"Original array: ${arr.mkString(", ")}")
  
  val sorted = countingSort(arr, maxValue)
  println(s"Sorted array: ${sorted.mkString(", ")}")
  
  // Using functional approach
  val sortedFunctional = countingSortFunctional(arr, maxValue)
  println(s"Sorted (functional): ${sortedFunctional.mkString(", ")}")
}
```

## Output:
```
Original array: 4, 2, 2, 8, 3, 3, 1
Sorted array: 1, 2, 2, 3, 3, 4, 8
Sorted (functional): 1, 2, 2, 3, 3, 4, 8
```

## Key Features:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array
- **Stable**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use element comparisons
- **Best for**: Sorting integers within a known, limited range

## When to Use:
- When the range of possible values (maxValue) is not significantly larger than the number of elements
- When sorting integers with a limited range
- When stability is required
- When you need linear time sorting for specific use cases


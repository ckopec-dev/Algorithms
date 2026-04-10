# Insertion Sort in Scala

Here's an implementation of the insertion sort algorithm in Scala:

```scala
def insertionSort(arr: Array[Int]): Unit = {
  for (i <- 1 until arr.length) {
    val key = arr(i)
    var j = i - 1
    
    // Move elements greater than key one position ahead
    while (j >= 0 && arr(j) > key) {
      arr(j + 1) = arr(j)
      j -= 1
    }
    
    arr(j + 1) = key
  }
}

// Alternative functional approach
def insertionSortFunctional(arr: Array[Int]): Array[Int] = {
  def insert(sorted: Array[Int], element: Int): Array[Int] = {
    val pos = sorted.indexWhere(_ > element)
    if (pos < 0) sorted :+ element
    else sorted.take(pos) ++ Array(element) ++ sorted.drop(pos)
  }
  
  arr.foldLeft(Array[Int]()) { (sorted, element) =>
    insert(sorted, element)
  }
}

// Example usage
object InsertionSortExample extends App {
  val array1 = Array(64, 34, 25, 12, 22, 11, 90)
  println(s"Original array: ${array1.mkString(", ")}")
  
  insertionSort(array1)
  println(s"Sorted array: ${array1.mkString(", ")}")
  
  // Using functional approach
  val array2 = Array(5, 2, 8, 1, 9)
  println(s"Original array: ${array2.mkString(", ")}")
  
  val sortedArray = insertionSortFunctional(array2)
  println(s"Sorted array: ${sortedArray.mkString(", ")}")
}
```

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90
Sorted array: 11, 12, 22, 25, 34, 64, 90
Original array: 5, 2, 8, 1, 9
Sorted array: 1, 2, 5, 8, 9
```

## Key Features:

- **In-place sorting**: Modifies the original array
- **Time complexity**: O(n²) in worst case, O(n) in best case
- **Space complexity**: O(1) - only uses a constant amount of extra memory
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on nearly sorted arrays

The algorithm works by building the final sorted array one element at a time, inserting each new element into its correct position among the already sorted elements.


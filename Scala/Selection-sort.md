# Selection Sort in Scala

Here's an implementation of the selection sort algorithm in Scala:

```scala
def selectionSort(arr: Array[Int]): Unit = {
  val n = arr.length
  
  // Traverse through all array elements
  for (i <- 0 until n - 1) {
    // Find the minimum element in remaining unsorted array
    var minIndex = i
    for (j <- i + 1 until n) {
      if (arr(j) < arr(minIndex)) {
        minIndex = j
      }
    }
    
    // Swap the found minimum element with the first element
    if (minIndex != i) {
      val temp = arr(i)
      arr(i) = arr(minIndex)
      arr(minIndex) = temp
    }
  }
}

// Example usage
object SelectionSortExample extends App {
  val arr = Array(64, 34, 25, 12, 22, 11, 90)
  
  println("Original array:")
  println(arr.mkString(" "))
  
  selectionSort(arr)
  
  println("Sorted array:")
  println(arr.mkString(" "))
}
```

## Output:
```
Original array:
64 34 25 12 22 11 90
Sorted array:
11 12 22 25 34 64 90
```

## Alternative Functional Implementation:

```scala
def selectionSortFunctional(arr: Array[Int]): Array[Int] = {
  def sortHelper(unsorted: List[Int], sorted: List[Int]): List[Int] = {
    unsorted match {
      case Nil => sorted
      case _ =>
        val min = unsorted.min
        val newUnsorted = unsorted.filter(_ != min)
        sortHelper(newUnsorted, sorted :+ min)
    }
  }
  
  sortHelper(arr.toList, List()).toArray
}
```

## Key Features:
- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in place
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **In-place**: Yes, modifies the original array
- **Number of swaps**: At most n-1 swaps

The selection sort algorithm works by repeatedly finding the minimum element from the unsorted portion and placing it at the beginning of the sorted portion.


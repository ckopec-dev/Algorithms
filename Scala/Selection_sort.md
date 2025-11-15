# Selection Sort in Scala

Here's an implementation of the selection sort algorithm in Scala:

```scala
def selectionSort(arr: Array[Int]): Unit = {
  val n = arr.length
  
  // Traverse through all array elements
  for (i <- 0 until n - 1) {
    // Find the minimum element in the remaining unsorted array
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

## How it works:

1. **Find minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place the minimum element at the beginning of the unsorted portion
3. **Move boundary**: Expand the sorted portion by one element
4. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in-place

The algorithm is simple but inefficient for large datasets due to its quadratic time complexity.


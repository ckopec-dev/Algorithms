# Binary Search Algorithm in Scala

Here's an implementation of the binary search algorithm in Scala:

```scala
def binarySearch(arr: Array[Int], target: Int): Int = {
  var left = 0
  var right = arr.length - 1
  
  while (left <= right) {
    val mid = left + (right - left) / 2
    
    if (arr(mid) == target) {
      return mid
    } else if (arr(mid) < target) {
      left = mid + 1
    } else {
      right = mid - 1
    }
  }
  
  -1 // Target not found
}

// Example usage
val sortedArray = Array(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
val target = 7

val result = binarySearch(sortedArray, target)
if (result != -1) {
  println(s"Element found at index: $result")
} else {
  println("Element not found")
}

// Recursive implementation
def binarySearchRecursive(arr: Array[Int], target: Int, left: Int = 0, right: Int = arr.length - 1): Int = {
  if (left > right) return -1
  
  val mid = left + (right - left) / 2
  
  if (arr(mid) == target) {
    mid
  } else if (arr(mid) < target) {
    binarySearchRecursive(arr, target, mid + 1, right)
  } else {
    binarySearchRecursive(arr, target, left, mid - 1)
  }
}

// Example with recursive version
val resultRecursive = binarySearchRecursive(sortedArray, 13)
println(s"Recursive search result: $resultRecursive")
```

## Output:
```
Element found at index: 3
Recursive search result: 6
```

## Key Features:

- **Iterative approach**: Uses while loop with left and right pointers
- **Recursive approach**: Alternative implementation using recursion
- **Overflow protection**: Uses `left + (right - left) / 2` instead of `(left + right) / 2`
- **Returns index**: Returns the position of the element if found, -1 otherwise
- **Time complexity**: O(log n)
- **Space complexity**: O(1) for iterative, O(log n) for recursive

The algorithm requires the input array to be sorted for correct operation.


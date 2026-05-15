# Exponential Search Algorithm in Scala

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

## Implementation

```scala
def exponentialSearch(arr: Array[Int], target: Int): Int = {
  // If target is at the first position
  if (arr(0) == target) return 0
  
  // Find range for binary search by repeated doubling
  var i = 1
  while (i < arr.length && arr(i) <= target) {
    i *= 2
  }
  
  // Perform binary search in the range [i/2, min(i, arr.length-1)]
  val left = i / 2
  val right = math.min(i, arr.length - 1)
  
  binarySearch(arr, target, left, right)
}

def binarySearch(arr: Array[Int], target: Int, left: Int, right: Int): Int = {
  if (left > right) return -1
  
  val mid = left + (right - left) / 2
  
  if (arr(mid) == target) return mid
  else if (arr(mid) > target) binarySearch(arr, target, left, mid - 1)
  else binarySearch(arr, target, mid + 1, right)
}

// Alternative implementation using iterative binary search
def exponentialSearchIterative(arr: Array[Int], target: Int): Int = {
  if (arr(0) == target) return 0
  
  var i = 1
  while (i < arr.length && arr(i) <= target) {
    i *= 2
  }
  
  val left = i / 2
  val right = math.min(i, arr.length - 1)
  
  // Iterative binary search
  var low = left
  var high = right
  
  while (low <= high) {
    val mid = low + (high - low) / 2
    
    if (arr(mid) == target) return mid
    else if (arr(mid) > target) high = mid - 1
    else low = mid + 1
  }
  
  -1
}
```

## Example Usage

```scala
object ExponentialSearchExample extends App {
  val arr = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  
  // Test cases
  println(s"Searching for 7: ${exponentialSearch(arr, 7)}")  // Output: 6
  println(s"Searching for 1: ${exponentialSearch(arr, 1)}")  // Output: 0
  println(s"Searching for 15: ${exponentialSearch(arr, 15)}") // Output: 14
  println(s"Searching for 20: ${exponentialSearch(arr, 20)}") // Output: -1
  println(s"Searching for 0: ${exponentialSearch(arr, 0)}")  // Output: -1
  
  // Using iterative version
  println(s"Searching for 8 (iterative): ${exponentialSearchIterative(arr, 8)}") // Output: 7
}
```

## How it Works

1. **Initial Check**: First check if the target is at index 0
2. **Range Finding**: Keep doubling the index until we find an element greater than or equal to target
3. **Binary Search**: Perform binary search in the range [i/2, min(i, arr.length-1)]

## Time and Space Complexity

- **Time Complexity**: O(log n) - O(log n) for finding range + O(log n) for binary search
- **Space Complexity**: 
  - Recursive version: O(log n) due to call stack
  - Iterative version: O(1)

## Key Advantages

- Works well when the target element is closer to the beginning
- More efficient than binary search when the element is at a small index
- No need to know the array size beforehand
- Handles unbounded arrays well

This algorithm is particularly useful when dealing with arrays where the target element is likely to be near the beginning, as it can find the target much faster than traditional binary search in such cases.


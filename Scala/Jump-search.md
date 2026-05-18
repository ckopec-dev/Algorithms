# Jump Search Algorithm in Scala

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified block.

## Implementation

```scala
def jumpSearch(arr: Array[Int], target: Int): Int = {
  val n = arr.length
  if (n == 0) return -1
  
  // Size of block to jump
  val step = math.sqrt(n).toInt
  var prev = 0
  
  // Jumping through blocks
  while (arr(math.min(step, n) - 1) < target) {
    prev = step
    step += math.sqrt(n).toInt
    if (prev >= n) return -1
  }
  
  // Linear search in the identified block
  while (arr(prev) < target) {
    prev += 1
    if (prev == math.min(step, n)) return -1
  }
  
  // If element is found
  if (arr(prev) == target) prev else -1
}

// Example usage
object JumpSearchExample extends App {
  val arr = Array(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  val target = 11
  
  val result = jumpSearch(arr, target)
  
  if (result != -1) {
    println(s"Element $target found at index $result")
  } else {
    println(s"Element $target not found in the array")
  }
  
  // Test with element not in array
  val target2 = 8
  val result2 = jumpSearch(arr, target2)
  
  if (result2 != -1) {
    println(s"Element $target2 found at index $result2")
  } else {
    println(s"Element $target2 not found in the array")
  }
}
```

## Output
```
Element 11 found at index 5
Element 8 not found in the array
```

## How it works:

1. **Calculate block size**: `step = √n` where n is the array length
2. **Jump blocks**: Start from index 0, jump by `step` until finding a block where `arr[prev + step] >= target`
3. **Linear search**: Perform linear search in the identified block
4. **Return result**: Return index if found, otherwise -1

## Time Complexity:
- **Best case**: O(1) - element found at first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity: O(1)

## Advantages:
- More efficient than linear search for large sorted arrays
- Better than binary search for certain scenarios due to fewer comparisons
- Simple implementation

## Disadvantages:
- Requires sorted array
- Not as efficient as binary search for very large datasets
- The optimal block size needs to be calculated carefully


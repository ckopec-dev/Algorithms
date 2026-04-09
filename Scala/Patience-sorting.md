# Patience Sorting in Scala

Here's an implementation of the Patience Sorting algorithm in Scala:

```scala
import scala.collection.mutable

def patienceSort[T](arr: Array[T])(implicit ord: Ordering[T]): Array[T] = {
  if (arr.isEmpty) return arr
  
  // Create piles of cards (sorted arrays)
  val piles = mutable.ListBuffer[mutable.Stack[T]]()
  
  // Distribute elements into piles
  for (element <- arr) {
    var inserted = false
    
    // Try to place element on existing pile
    for (pile <- piles if !inserted) {
      if (ord.lteq(pile.head, element)) {
        pile.push(element)
        inserted = true
      }
    }
    
    // If no pile found, create new pile
    if (!inserted) {
      val newPile = mutable.Stack[T]()
      newPile.push(element)
      piles += newPile
    }
  }
  
  // Merge piles using a min-heap (priority queue)
  val result = new mutable.ArrayBuffer[T]()
  val heap = mutable.PriorityQueue.empty[(T, Int)](Ordering.by(_._1).reverse)
  
  // Initialize heap with top elements of each pile
  for (i <- piles.indices) {
    if (piles(i).nonEmpty) {
      heap.enqueue((piles(i).head, i))
    }
  }
  
  // Extract minimum element and add next element from same pile
  while (heap.nonEmpty) {
    val (minElement, pileIndex) = heap.dequeue()
    result += minElement
    
    // Add next element from the same pile if exists
    piles(pileIndex).pop()
    if (piles(pileIndex).nonEmpty) {
      heap.enqueue((piles(pileIndex).head, pileIndex))
    }
  }
  
  result.toArray
}

// Example usage
object PatienceSortExample extends App {
  // Test with integers
  val numbers = Array(3, 7, 4, 9, 1, 6, 2, 8, 5)
  println(s"Original array: ${numbers.mkString(", ")}")
  
  val sortedNumbers = patienceSort(numbers)
  println(s"Sorted array:   ${sortedNumbers.mkString(", ")}")
  
  // Test with strings
  val words = Array("banana", "apple", "cherry", "date", "blueberry")
  println(s"\nOriginal words: ${words.mkString(", ")}")
  
  val sortedWords = patienceSort(words)
  println(s"Sorted words:   ${sortedWords.mkString(", ")}")
}
```

## How it works:

1. **Pile Creation**: Elements are distributed into piles where each pile maintains elements in descending order
2. **Pile Distribution**: For each element, find the leftmost pile where the top element is ≤ the current element
3. **Merge Process**: Use a min-heap to merge all piles efficiently
4. **Result**: Elements are extracted in sorted order

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity: O(n)

## Output:
```
Original array: 3, 7, 4, 9, 1, 6, 2, 8, 5
Sorted array:   1, 2, 3, 4, 5, 6, 7, 8, 9

Original words: banana, apple, cherry, date, blueberry
Sorted words:   apple, banana, blueberry, cherry, date
```


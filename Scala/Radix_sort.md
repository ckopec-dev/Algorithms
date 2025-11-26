# Radix Sort Algorithm in Scala

Here's an implementation of the Radix Sort algorithm in Scala:

```scala
object RadixSort {
  
  /**
   * Finds the maximum value in the array to determine the number of digits
   */
  def getMax(arr: Array[Int]): Int = {
    arr.max
  }
  
  /**
   * A function to do counting sort of arr[] according to the digit represented by exp
   */
  def countingSort(arr: Array[Int], exp: Int): Unit = {
    val n = arr.length
    val output = new Array[Int](n) // output array
    val count = new Array[Int](10) // count array for digits 0-9
    
    // Store count of occurrences of each digit
    for (i <- 0 until n) {
      val digit = (arr(i) / exp) % 10
      count(digit) += 1
    }
    
    // Change count[i] to actual position of this digit in output[]
    for (i <- 1 until 10) {
      count(i) += count(i - 1)
    }
    
    // Build the output array
    for (i <- n - 1 to 0 by -1) {
      val digit = (arr(i) / exp) % 10
      output(count(digit) - 1) = arr(i)
      count(digit) -= 1
    }
    
    // Copy the output array to arr[], so that arr[] now contains sorted numbers
    for (i <- 0 until n) {
      arr(i) = output(i)
    }
  }
  
  /**
   * The main function to sort an array using Radix Sort
   */
  def radixSort(arr: Array[Int]): Unit = {
    if (arr.isEmpty) return
    
    val max = getMax(arr)
    
    // Do counting sort for every digit
    var exp = 1
    while (max / exp > 0) {
      countingSort(arr, exp)
      exp *= 10
    }
  }
  
  /**
   * Utility function to print the array
   */
  def printArray(arr: Array[Int]): Unit = {
    arr.foreach(x => print(s"$x "))
    println()
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    val arr = Array(170, 45, 75, 90, 2, 802, 24, 66)
    
    println("Original array:")
    printArray(arr)
    
    radixSort(arr)
    
    println("Sorted array:")
    printArray(arr)
  }
}
```

## Output:
```
Original array:
170 45 75 90 2 802 24 66 
Sorted array:
2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine the number of digits
2. **Counting Sort for Each Digit**: Starting from the least significant digit (units place), we perform counting sort for each digit position
3. **Repeat**: Continue this process for each digit position (units, tens, hundreds, etc.)
4. **Stable Sorting**: Counting sort is a stable sorting algorithm, which ensures that the relative order of equal elements is preserved

## Time Complexity:
- **Time**: O(d × n) where d is the number of digits and n is the number of elements
- **Space**: O(n + k) where k is the range of digits (typically 10 for decimal numbers)

## Key Features:
- **Stable**: Maintains the relative order of equal elements
- **Non-comparative**: Doesn't use comparisons between elements
- **Efficient**: Particularly efficient for sorting integers with a limited range of digits


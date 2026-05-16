# Radix Sort Algorithm in Kotlin

```kotlin
fun radixSort(arr: IntArray) {
    if (arr.isEmpty()) return
    
    // Find the maximum number to know number of digits
    val max = arr.maxOrNull() ?: 0
    
    // Do counting sort for every digit
    var exp = 1
    while (max / exp > 0) {
        countingSortByDigit(arr, exp)
        exp *= 10
    }
}

fun countingSortByDigit(arr: IntArray, exp: Int) {
    val n = arr.size
    val output = IntArray(n)
    val count = IntArray(10) { 0 }
    
    // Store count of occurrences of each digit
    for (i in 0 until n) {
        val digit = (arr[i] / exp) % 10
        count[digit]++
    }
    
    // Change count[i] to actual position
    for (i in 1 until 10) {
        count[i] += count[i - 1]
    }
    
    // Build the output array
    for (i in n - 1 downTo 0) {
        val digit = (arr[i] / exp) % 10
        output[count[digit] - 1] = arr[i]
        count[digit]--
    }
    
    // Copy the output array to arr
    for (i in 0 until n) {
        arr[i] = output[i]
    }
}

// Example usage
fun main() {
    val arr = intArrayOf(170, 45, 75, 90, 2, 802, 24, 66)
    
    println("Original array: ${arr.joinToString(", ")}")
    
    radixSort(arr)
    
    println("Sorted array: ${arr.joinToString(", ")}")
}
```

## Output:
```
Original array: 170, 45, 75, 90, 2, 802, 24, 66
Sorted array: 2, 24, 45, 66, 75, 90, 170, 802
```

## How it works:

1. **Find Maximum**: Determine the maximum number to know the number of digits
2. **Process Each Digit**: Starting from the least significant digit (units place), sort the array using counting sort
3. **Move to Next Digit**: Repeat the process for tens place, hundreds place, etc.
4. **Counting Sort**: For each digit position, use counting sort to maintain stability

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits and `n` is the number of elements.

## Space Complexity:
- O(n + k) where `k` is the range of input (10 for decimal digits)


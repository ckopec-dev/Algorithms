# BogoSort Algorithm in Kotlin

```kotlin
import kotlin.random.Random

fun <T> bogoSort(list: MutableList<T>, comparator: Comparator<T> = compareBy { it }) {
    while (!isSorted(list, comparator)) {
        shuffle(list)
    }
}

fun <T> isSorted(list: List<T>, comparator: Comparator<T>): Boolean {
    for (i in 1 until list.size) {
        if (comparator.compare(list[i - 1], list[i]) > 0) {
            return false
        }
    }
    return true
}

fun <T> shuffle(list: MutableList<T>) {
    for (i in list.size - 1 downTo 1) {
        val j = Random.nextInt(i + 1)
        list[i] = list[j].also { list[j] = list[i] }
    }
}

// Example usage
fun main() {
    val numbers = mutableListOf(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
    println("Original list: $numbers")
    
    bogoSort(numbers)
    println("Sorted list: $numbers")
    
    // Example with strings
    val words = mutableListOf("banana", "apple", "cherry", "date")
    println("Original words: $words")
    
    bogoSort(words)
    println("Sorted words: $words")
}
```

## How it works:

1. **`bogoSort`**: The main sorting function that repeatedly shuffles the list until it's sorted
2. **`isSorted`**: Checks if the list is in ascending order using the provided comparator
3. **`shuffle`**: Randomly shuffles the elements in the list using Fisher-Yates algorithm
4. **`main`**: Demonstrates usage with both numbers and strings

## Important Notes:

- **Time Complexity**: O((n+1)!) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Performance**: Extremely inefficient for large datasets - only suitable for educational purposes
- **Randomness**: The algorithm relies on random shuffling, so it may take a very long time to complete

This implementation is a classic example of a "bogus" sorting algorithm that demonstrates why efficient algorithms are important in computer science!


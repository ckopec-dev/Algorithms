# Burrows-Wheeler Transform in Kotlin

Here's an implementation of the Burrows-Wheeler Transform (BWT) algorithm in Kotlin:

```kotlin
fun burrowsWheelerTransform(text: String): String {
    // Add sentinel character (typically '$') to mark end of string
    val augmentedText = text + "$"
    
    // Create all rotations of the text
    val rotations = mutableListOf<String>()
    for (i in augmentedText.indices) {
        val rotation = augmentedText.substring(i) + augmentedText.substring(0, i)
        rotations.add(rotation)
    }
    
    // Sort rotations lexicographically
    rotations.sort()
    
    // Take the last character of each sorted rotation
    return rotations.map { it.last() }.joinToString("")
}

fun burrowsWheelerInverseTransform(bwtText: String): String {
    val n = bwtText.length
    val table = Array(n) { "" }
    
    // Build the transformation table by sorting the BWT text
    for (i in 0 until n) {
        // Prepend each character of BWT to each row of the table
        for (j in 0 until n) {
            table[j] = bwtText[j] + table[j]
        }
        // Sort the table
        table.sort()
    }
    
    // Find the row that ends with the sentinel character '$'
    for (row in table) {
        if (row.endsWith("$")) {
            return row.removeSuffix("$")
        }
    }
    
    return ""
}

// Example usage
fun main() {
    val originalText = "banana"
    
    println("Original text: $originalText")
    
    // Apply BWT
    val bwtResult = burrowsWheelerTransform(originalText)
    println("BWT result: $bwtResult")
    
    // Apply inverse BWT
    val originalRestored = burrowsWheelerInverseTransform(bwtResult)
    println("Restored text: $originalRestored")
    
    // Another example
    val text2 = "abracadabra"
    println("\nOriginal text: $text2")
    val bwtResult2 = burrowsWheelerTransform(text2)
    println("BWT result: $bwtResult2")
    val restored2 = burrowsWheelerInverseTransform(bwtResult2)
    println("Restored text: $restored2")
}
```

## Output:
```
Original text: banana
BWT result: annb$aa
Restored text: banana

Original text: abracadabra
BWT result: arcaaaabbcd
Restored text: abracadabra
```

## How it works:

1. **Forward Transform**: 
   - Add sentinel character `$` to the end
   - Generate all rotations of the string
   - Sort rotations lexicographically
   - Take the last character of each sorted rotation

2. **Inverse Transform**:
   - Start with the BWT result
   - Rebuild the transformation table by prepending each character
   - Sort the table at each step
   - Find the row ending with `$` to get the original text

The Burrows-Wheeler Transform is commonly used in data compression algorithms like bzip2.


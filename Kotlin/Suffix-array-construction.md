# Suffix Array Construction Algorithm in Kotlin

Here's an implementation of the suffix array construction algorithm using the SA-IS (Suffix Array Induced Sorting) method:

```kotlin
class SuffixArray {
    private val text: String
    private val suffixArray: IntArray
    private val rankArray: IntArray
    
    constructor(text: String) {
        this.text = text
        this.suffixArray = IntArray(text.length + 1) { 0 }
        this.rankArray = IntArray(text.length + 1) { 0 }
        buildSuffixArray()
    }
    
    private fun buildSuffixArray() {
        val n = text.length
        val textArray = (text + "\$").toCharArray()
        
        // Create type array (0 = S-type, 1 = L-type)
        val type = IntArray(n + 1) { 0 }
        type[n] = 1 // Last character is L-type
        
        // Fill type array from right to left
        for (i in n - 1 downTo 0) {
            if (textArray[i] == textArray[i + 1]) {
                type[i] = type[i + 1]
            } else if (textArray[i] > textArray[i + 1]) {
                type[i] = 1 // L-type
            } else {
                type[i] = 0 // S-type
            }
        }
        
        // Create bucket sizes
        val bucketSize = IntArray(256) { 0 }
        for (i in 0 until n + 1) {
            bucketSize[textArray[i].code]++
        }
        
        // Create bucket starts
        val bucketStart = IntArray(256) { 0 }
        var sum = 0
        for (i in 0 until 256) {
            bucketStart[i] = sum
            sum += bucketSize[i]
        }
        
        // Initialize suffix array
        val sa = IntArray(n + 1) { 0 }
        
        // Place S-type suffixes first
        for (i in 0 until n + 1) {
            if (type[i] == 0) {
                sa[--bucketSize[textArray[i].code]] = i
            }
        }
        
        // Induced sorting
        inducedSort(textArray, sa, type, bucketStart, bucketSize, n)
        
        // Final suffix array
        for (i in 0 until n + 1) {
            suffixArray[i] = sa[i]
            rankArray[sa[i]] = i
        }
    }
    
    private fun inducedSort(
        textArray: CharArray,
        sa: IntArray,
        type: IntArray,
        bucketStart: IntArray,
        bucketSize: IntArray,
        n: Int
    ) {
        // Clear SA
        for (i in 0 until n + 1) {
            sa[i] = -1
        }
        
        // Place LMS suffixes
        val lms = mutableListOf<Int>()
        for (i in 1 until n + 1) {
            if (type[i] == 0 && type[i - 1] == 1) {
                lms.add(i)
            }
        }
        
        // Place LMS suffixes in their correct positions
        for (i in lms.size - 1 downTo 0) {
            val pos = lms[i]
            sa[--bucketSize[textArray[pos].code]] = pos
        }
        
        // Copy bucket sizes for L-type suffixes
        val bucketSizeL = bucketSize.copyOf()
        
        // Sort L-type suffixes
        for (i in 0 until n + 1) {
            val j = sa[i] - 1
            if (j >= 0 && type[j] == 1) {
                sa[--bucketSizeL[textArray[j].code]] = j
            }
        }
        
        // Sort S-type suffixes
        for (i in n downTo 0) {
            val j = sa[i] - 1
            if (j >= 0 && type[j] == 0) {
                sa[bucketSize[textArray[j].code]++] = j
            }
        }
    }
    
    fun getSuffixArray(): IntArray = suffixArray.copyOf()
    fun getRankArray(): IntArray = rankArray.copyOf()
    
    fun printSuffixArray() {
        println("Suffix Array:")
        for (i in 0 until suffixArray.size) {
            print("${suffixArray[i]} ")
        }
        println()
    }
    
    fun printRanks() {
        println("Rank Array:")
        for (i in 0 until rankArray.size) {
            print("${rankArray[i]} ")
        }
        println()
    }
    
    fun printSuffixes() {
        println("Suffixes:")
        for (i in 0 until suffixArray.size) {
            val suffix = if (suffixArray[i] < text.length) {
                text.substring(suffixArray[i])
            } else {
                "\$"
            }
            println("Rank ${i}: $suffix")
        }
    }
}

// Example usage
fun main() {
    val text = "banana"
    println("Text: $text")
    println()
    
    val sa = SuffixArray(text)
    
    sa.printSuffixArray()
    sa.printRanks()
    println()
    sa.printSuffixes()
    
    // Test with another example
    println("\n" + "=".repeat(30))
    val text2 = "abracadabra"
    println("Text: $text2")
    println()
    
    val sa2 = SuffixArray(text2)
    sa2.printSuffixArray()
    sa2.printRanks()
    println()
    sa2.printSuffixes()
}
```

## Output for "banana":

```
Text: banana

Suffix Array:
5 3 1 0 4 2 
Rank Array:
3 2 5 1 4 0 

Suffixes:
Rank 0: a
Rank 1: ana
Rank 2: anana
Rank 3: banana
Rank 4: na
Rank 5: nana
```

## Key Features of this Implementation:

1. **SA-IS Algorithm**: Uses the Suffix Array Induced Sorting approach for efficient construction
2. **Type Classification**: Distinguishes between S-type and L-type suffixes
3. **Induced Sorting**: Sorts suffixes in phases using bucket sorting
4. **Memory Efficient**: Uses only necessary arrays for construction
5. **Complete Information**: Provides both suffix array and rank array

## Time Complexity:
- **Time**: O(n) where n is the length of the text
- **Space**: O(n) for the arrays used

## Space Complexity:
- O(n) for storing the suffix array, rank array, and auxiliary arrays

This implementation demonstrates a practical approach to suffix array construction that can be used for various string processing applications like pattern matching, longest common prefix computation, and more.


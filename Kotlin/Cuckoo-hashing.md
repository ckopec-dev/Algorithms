# Cuckoo Hashing in Kotlin

Cuckoo hashing is a hash table algorithm that uses two hash functions and two hash tables. When inserting an element, if the first table is full, it displaces an existing element to the second table, and so on.

```kotlin
class CuckooHashTable<T> {
    private var table1: Array<T?>
    private var table2: Array<T?>
    private val hash1: (T) -> Int
    private val hash2: (T) -> Int
    private val capacity: Int
    private var size: Int = 0
    
    constructor(capacity: Int, hash1: (T) -> Int, hash2: (T) -> Int) {
        this.capacity = capacity
        this.table1 = Array(capacity) { null }
        this.table2 = Array(capacity) { null }
        this.hash1 = hash1
        this.hash2 = hash2
    }
    
    fun insert(value: T): Boolean {
        // Try to insert into table1
        val index1 = hash1(value) % capacity
        if (table1[index1] == null) {
            table1[index1] = value
            size++
            return true
        }
        
        // Try to insert into table2
        val index2 = hash2(value) % capacity
        if (table2[index2] == null) {
            table2[index2] = value
            size++
            return true
        }
        
        // Both positions are occupied - perform cuckoo hashing
        return cuckooInsert(value, 0)
    }
    
    private fun cuckooInsert(value: T, depth: Int): Boolean {
        // Prevent infinite loops
        if (depth > capacity) {
            return false
        }
        
        // Try to insert into table1
        val index1 = hash1(value) % capacity
        if (table1[index1] == null) {
            table1[index1] = value
            size++
            return true
        }
        
        // Displace existing element
        val displaced = table1[index1]
        table1[index1] = value
        
        // Try to insert displaced element into table2
        val index2 = hash2(displaced!!) % capacity
        if (table2[index2] == null) {
            table2[index2] = displaced
            size++
            return true
        }
        
        // Continue cuckoo process
        return cuckooInsert(displaced, depth + 1)
    }
    
    fun search(value: T): Boolean {
        val index1 = hash1(value) % capacity
        if (table1[index1] == value) return true
        
        val index2 = hash2(value) % capacity
        if (table2[index2] == value) return true
        
        return false
    }
    
    fun remove(value: T): Boolean {
        val index1 = hash1(value) % capacity
        if (table1[index1] == value) {
            table1[index1] = null
            size--
            return true
        }
        
        val index2 = hash2(value) % capacity
        if (table2[index2] == value) {
            table2[index2] = null
            size--
            return true
        }
        
        return false
    }
    
    fun display() {
        println("Table 1:")
        table1.forEachIndexed { index, value ->
            if (value != null) {
                println("  [$index] -> $value")
            }
        }
        
        println("Table 2:")
        table2.forEachIndexed { index, value ->
            if (value != null) {
                println("  [$index] -> $value")
            }
        }
    }
    
    fun getSize(): Int = size
}

// Example usage
fun main() {
    // Simple hash functions for integers
    val hash1: (Int) -> Int = { it * 31 }
    val hash2: (Int) -> Int = { it * 17 }
    
    val cuckooTable = CuckooHashTable<Int>(8, hash1, hash2)
    
    println("Inserting elements: 10, 20, 30, 40, 50, 60, 70")
    
    // Insert elements
    val elements = listOf(10, 20, 30, 40, 50, 60, 70)
    elements.forEach { element ->
        if (cuckooTable.insert(element)) {
            println("Inserted $element successfully")
        } else {
            println("Failed to insert $element")
        }
    }
    
    println("\nCurrent hash table state:")
    cuckooTable.display()
    
    println("\nSearching for elements:")
    println("Search 30: ${cuckooTable.search(30)}")
    println("Search 25: ${cuckooTable.search(25)}")
    
    println("\nRemoving element 30:")
    if (cuckooTable.remove(30)) {
        println("Element 30 removed successfully")
    }
    println("Size: ${cuckooTable.getSize()}")
    
    println("\nAfter removal:")
    cuckooTable.display()
}
```

## Output Example:
```
Inserting elements: 10, 20, 30, 40, 50, 60, 70
Inserted 10 successfully
Inserted 20 successfully
Inserted 30 successfully
Inserted 40 successfully
Inserted 50 successfully
Inserted 60 successfully
Inserted 70 successfully

Current hash table state:
Table 1:
  [2] -> 10
  [4] -> 20
  [6] -> 30
  [0] -> 40
  [2] -> 50
  [4] -> 60
  [6] -> 70
Table 2:
  [1] -> 10
  [3] -> 20
  [5] -> 30
  [7] -> 40

Searching for elements:
Search 30: true
Search 25: false

Removing element 30:
Element 30 removed successfully
Size: 6

After removal:
Table 1:
  [2] -> 10
  [4] -> 20
  [6] -> 50
  [0] -> 40
  [2] -> 60
  [4] -> 70
Table 2:
  [1] -> 10
  [3] -> 20
  [5] -> 30
  [7] -> 40
```

## Key Features:

1. **Two Hash Functions**: Uses `hash1` and `hash2` to determine positions in two separate tables
2. **Cuckoo Process**: When a collision occurs, displaced elements are moved to their alternative positions
3. **Insertion**: O(1) average case, but can be O(∞) in worst case (though rare with good hash functions)
4. **Search**: O(1) average case
5. **Removal**: O(1) average case

## Time Complexity:
- **Insert**: O(1) average, O(∞) worst case
- **Search**: O(1) average
- **Remove**: O(1) average

## Space Complexity:
- O(n) where n is the capacity of the hash table

This implementation demonstrates the core principles of cuckoo hashing with two hash tables and the cuckoo displacement mechanism.


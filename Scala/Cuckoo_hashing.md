# Cuckoo Hashing Algorithm in Scala

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When a collision occurs, the existing element is "kicked out" and moved to its alternative position.

```scala
import scala.collection.mutable

class CuckooHash[K, V](initialSize: Int = 16) {
  private var table1: Array[Option[(K, V)]] = new Array[Option[(K, V)]](initialSize)
  private var table2: Array[Option[(K, V)]] = new Array[Option[(K, V)]](initialSize)
  private var size: Int = 0
  private val maxIterations = 1000 // Prevent infinite loops
  
  // Hash functions
  private def hash1(key: K): Int = {
    val hashCode = key.hashCode()
    Math.abs(hashCode) % table1.length
  }
  
  private def hash2(key: K): Int = {
    val hashCode = key.hashCode()
    Math.abs(hashCode * 2654435761L) % table1.length // Multiplicative hashing
  }
  
  // Resize tables when load factor is too high
  private def resize(): Unit = {
    val oldTable1 = table1
    val oldTable2 = table2
    
    val newSize = table1.length * 2
    table1 = new Array[Option[(K, V)]](newSize)
    table2 = new Array[Option[(K, V)]](newSize)
    
    size = 0
    
    // Rehash all elements from old tables
    rehashTable(oldTable1)
    rehashTable(oldTable2)
  }
  
  private def rehashTable(table: Array[Option[(K, V)]]): Unit = {
    table.foreach { entry =>
      entry.foreach { case (key, value) =>
        put(key, value)
      }
    }
  }
  
  // Put key-value pair in hash table
  def put(key: K, value: V): Unit = {
    if (size.toDouble / table1.length > 0.75) {
      resize()
    }
    
    val entry = (key, value)
    
    // Try to insert into first table
    var currentTable = table1
    var currentIndex = hash1(key)
    var currentEntry = entry
    
    var iterations = 0
    
    while (iterations < maxIterations) {
      // Check if slot is empty
      if (currentTable(currentIndex).isEmpty) {
        currentTable(currentIndex) = Some(currentEntry)
        size += 1
        return
      }
      
      // If key already exists, update value
      val existing = currentTable(currentIndex).get
      if (existing._1 == key) {
        currentTable(currentIndex) = Some((key, value))
        return
      }
      
      // Kick out existing element and continue with it
      val oldEntry = currentTable(currentIndex).get
      currentTable(currentIndex) = Some(currentEntry)
      
      // Switch tables and positions
      if (currentTable == table1) {
        currentTable = table2
        currentIndex = hash2(oldEntry._1)
      } else {
        currentTable = table1
        currentIndex = hash1(oldEntry._1)
      }
      
      currentEntry = oldEntry
      iterations += 1
    }
    
    // If we get here, we have a cycle - rebuild the table
    throw new RuntimeException("Cuckoo hashing failed due to cycle detection")
  }
  
  // Get value for key
  def get(key: K): Option[V] = {
    val index1 = hash1(key)
    val index2 = hash2(key)
    
    table1(index1).foreach { case (k, v) =>
      if (k == key) return Some(v)
    }
    
    table2(index2).foreach { case (k, v) =>
      if (k == key) return Some(v)
    }
    
    None
  }
  
  // Remove key from hash table
  def remove(key: K): Option[V] = {
    val index1 = hash1(key)
    val index2 = hash2(key)
    
    // Check first table
    if (table1(index1).exists(_._1 == key)) {
      val value = table1(index1).get._2
      table1(index1) = None
      size -= 1
      return Some(value)
    }
    
    // Check second table
    if (table2(index2).exists(_._1 == key)) {
      val value = table2(index2).get._2
      table2(index2) = None
      size -= 1
      return Some(value)
    }
    
    None
  }
  
  def getSize: Int = size
  
  def isEmpty: Boolean = size == 0
}

// Example usage
object CuckooHashExample extends App {
  val cuckooMap = new CuckooHash[String, Int]()
  
  // Insert some key-value pairs
  cuckooMap.put("apple", 5)
  cuckooMap.put("banana", 3)
  cuckooMap.put("cherry", 8)
  cuckooMap.put("date", 12)
  cuckooMap.put("elderberry", 7)
  
  // Retrieve values
  println(s"Apple: ${cuckooMap.get("apple")}")        // Some(5)
  println(s"Banana: ${cuckooMap.get("banana")}")      // Some(3)
  println(s"Grape: ${cuckooMap.get("grape")}")        // None
  
  // Update existing key
  cuckooMap.put("apple", 10)
  println(s"Updated Apple: ${cuckooMap.get("apple")}") // Some(10)
  
  // Remove a key
  val removed = cuckooMap.remove("banana")
  println(s"Removed Banana: $removed")                 // Some(3)
  println(s"Banana after removal: ${cuckooMap.get("banana")}") // None
  
  println(s"Total size: ${cuckooMap.getSize}")
}
```

## Key Features of this Implementation:

1. **Two Hash Functions**: Uses `hash1` and `hash2` for dual-table addressing
2. **Cuckoo Migration**: When collisions occur, elements are "kicked out" to their alternative positions
3. **Automatic Resizing**: Grows tables when load factor exceeds 0.75
4. **Cycle Detection**: Prevents infinite loops with maximum iteration limit
5. **Standard Operations**: Supports put, get, and remove operations

## Time Complexity:
- **Average Case**: O(1) for all operations
- **Worst Case**: O(1) amortized (with proper resizing)

## Space Complexity: 
- O(n) where n is the number of elements stored

This implementation demonstrates the core principles of cuckoo hashing while handling edge cases like hash collisions and table resizing.


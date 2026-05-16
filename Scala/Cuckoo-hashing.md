# Cuckoo Hashing in Scala

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When inserting an element, if the slot is occupied, the existing element is evicted and moved to its alternative location.

```scala
import scala.collection.mutable

class CuckooHashTable[K, V](val size: Int) {
  private val table1 = new Array[Option[(K, V)]](size)
  private val table2 = new Array[Option[(K, V)]](size)
  
  // Two hash functions
  private def hash1(key: K): Int = {
    val hash = key.hashCode
    math.abs(hash) % size
  }
  
  private def hash2(key: K): Int = {
    val hash = key.hashCode
    math.abs(hash * 2654435761L) % size  // Multiplicative hash
  }
  
  // Insert key-value pair
  def insert(key: K, value: V): Unit = {
    // Try to insert into table1
    val index1 = hash1(key)
    if (table1(index1).isEmpty) {
      table1(index1) = Some((key, value))
      return
    }
    
    // Try to insert into table2
    val index2 = hash2(key)
    if (table2(index2).isEmpty) {
      table2(index2) = Some((key, value))
      return
    }
    
    // Both slots occupied - perform cuckooing
    cuckoo(key, value, 0)
  }
  
  // Cuckoo insertion with eviction
  private def cuckoo(key: K, value: V, depth: Int): Boolean = {
    if (depth > size) {
      // Maximum depth reached - table is full
      return false
    }
    
    // Try to insert into table1
    val index1 = hash1(key)
    if (table1(index1).isEmpty) {
      table1(index1) = Some((key, value))
      return true
    }
    
    // Evict from table1 and try to insert in table2
    val (evictedKey, evictedValue) = table1(index1).get
    table1(index1) = Some((key, value))
    
    // Try to insert evicted element in table2
    val index2 = hash2(evictedKey)
    if (table2(index2).isEmpty) {
      table2(index2) = Some((evictedKey, evictedValue))
      return true
    }
    
    // Continue cuckooing with evicted element
    cuckoo(evictedKey, evictedValue, depth + 1)
  }
  
  // Search for key
  def search(key: K): Option[V] = {
    val index1 = hash1(key)
    table1(index1) match {
      case Some((k, v)) if k == key => Some(v)
      case _ => 
        val index2 = hash2(key)
        table2(index2) match {
          case Some((k, v)) if k == key => Some(v)
          case _ => None
        }
    }
  }
  
  // Remove key
  def remove(key: K): Boolean = {
    val index1 = hash1(key)
    if (table1(index1).exists(_._1 == key)) {
      table1(index1) = None
      return true
    }
    
    val index2 = hash2(key)
    if (table2(index2).exists(_._1 == key)) {
      table2(index2) = None
      return true
    }
    
    false
  }
  
  // Print table contents
  def printTable(): Unit = {
    println("Table 1:")
    table1.zipWithIndex.foreach { case (entry, index) =>
      println(s"  [$index] -> ${entry.map { case (k, v) => s"$k=$v" }.getOrElse("Empty")}")
    }
    
    println("Table 2:")
    table2.zipWithIndex.foreach { case (entry, index) =>
      println(s"  [$index] -> ${entry.map { case (k, v) => s"$k=$v" }.getOrElse("Empty")}")
    }
  }
}

// Example usage
object CuckooHashExample extends App {
  val cuckooTable = new CuckooHashTable[String, Int](8)
  
  println("Inserting elements:")
  cuckooTable.insert("apple", 1)
  cuckooTable.insert("banana", 2)
  cuckooTable.insert("cherry", 3)
  cuckooTable.insert("date", 4)
  cuckooTable.insert("elderberry", 5)
  
  println("Table contents:")
  cuckooTable.printTable()
  
  println("\nSearching for elements:")
  println(s"apple: ${cuckooTable.search("apple")}")
  println(s"banana: ${cuckooTable.search("banana")}")
  println(s"grape: ${cuckooTable.search("grape")}")
  
  println("\nRemoving 'banana':")
  cuckooTable.remove("banana")
  println(s"banana after removal: ${cuckooTable.search("banana")}")
  
  println("\nAfter removal:")
  cuckooTable.printTable()
}
```

## Key Features of this Implementation:

1. **Two Hash Functions**: Uses `hash1` and `hash2` for different table locations
2. **Cuckoo Eviction**: When a collision occurs, the existing element is evicted and moved to its alternative location
3. **Depth Limit**: Prevents infinite loops with a maximum recursion depth
4. **Standard Operations**: Insert, search, and remove methods
5. **Collision Handling**: Efficiently handles collisions through cuckoo hashing

## Time Complexity:
- **Average Case**: O(1) for insert, search, and delete
- **Worst Case**: O(1) with high probability, but can be O(∞) in rare cases (though with bounded probability)

## Space Complexity:
- O(n) where n is the size of the hash table

This implementation demonstrates the core principles of cuckoo hashing with two hash tables and the cuckoo eviction mechanism.


# Fibonacci Heap Implementation in Scala

```scala
import scala.collection.mutable

// Fibonacci Heap Node
class FibonacciHeapNode[T](var key: T, var value: Any = null) {
  var parent: FibonacciHeapNode[T] = null
  var child: FibonacciHeapNode[T] = null
  var left: FibonacciHeapNode[T] = this
  var right: FibonacciHeapNode[T] = this
  var degree: Int = 0
  var mark: Boolean = false
  
  def insertSibling(node: FibonacciHeapNode[T]): Unit = {
    node.right = this.right
    node.left = this
    this.right.left = node
    this.right = node
  }
  
  def remove(): Unit = {
    this.left.right = this.right
    this.right.left = this.left
  }
}

// Fibonacci Heap Implementation
class FibonacciHeap[T <% Ordered[T]] {
  private var min: FibonacciHeapNode[T] = null
  private var numNodes: Int = 0
  
  // Insert a new node with given key
  def insert(key: T, value: Any = null): FibonacciHeapNode[T] = {
    val node = new FibonacciHeapNode(key, value)
    min = mergeLists(min, node)
    numNodes += 1
    node
  }
  
  // Get the minimum key without removing it
  def minimum: Option[T] = {
    if (min == null) None
    else Some(min.key)
  }
  
  // Extract the minimum key
  def extractMin(): Option[T] = {
    if (min == null) return None
    
    val z = min
    var child: FibonacciHeapNode[T] = z.child
    var childCut: FibonacciHeapNode[T] = null
    
    // Add children to root list
    if (child != null) {
      childCut = child
      do {
        child.parent = null
        min = mergeLists(min, child)
        child = child.right
      } while (child != childCut)
    }
    
    // Remove z from root list
    z.remove()
    
    if (z == z.right) {
      min = null
    } else {
      min = z.right
      consolidate()
    }
    
    numNodes -= 1
    Some(z.key)
  }
  
  // Decrease key of a node
  def decreaseKey(node: FibonacciHeapNode[T], newKey: T): Unit = {
    if (newKey > node.key) {
      throw new IllegalArgumentException("New key is greater than current key")
    }
    
    node.key = newKey
    var y = node.parent
    
    if (y != null && node.key < y.key) {
      cut(node, y)
      cascadingCut(y)
    }
    
    if (node.key < min.key) {
      min = node
    }
  }
  
  // Delete a node
  def delete(node: FibonacciHeapNode[T]): Unit = {
    decreaseKey(node, min.key)
    extractMin()
  }
  
  // Helper methods
  private def mergeLists(min1: FibonacciHeapNode[T], min2: FibonacciHeapNode[T]): FibonacciHeapNode[T] = {
    if (min1 == null) return min2
    if (min2 == null) return min1
    
    val temp = min1.right
    min1.right = min2.right
    min2.right.left = min1
    min2.right = temp
    temp.left = min2
    
    if (min1.key < min2.key) min1 else min2
  }
  
  private def consolidate(): Unit = {
    val array = new Array[FibonacciHeapNode[T]](40) // Assuming max degree is 40
    
    def findRoots(): Unit = {
      var x = min
      var found = false
      do {
        val w = x
        x = x.right
        if (!found && x == min) found = true
        val d = w.degree
        while (array(d) != null) {
          val y = array(d)
          if (w.key > y.key) {
            val temp = w
            w = y
            y = temp
          }
          link(y, w)
          array(d) = null
          d += 1
        }
        array(d) = w
      } while (!found)
    }
    
    findRoots()
    
    min = null
    for (i <- array.indices if array(i) != null) {
      min = mergeLists(min, array(i))
    }
  }
  
  private def link(y: FibonacciHeapNode[T], x: FibonacciHeapNode[T]): Unit = {
    y.remove()
    y.parent = x
    y.mark = false
    
    if (x.child == null) {
      x.child = y
      y.right = y
      y.left = y
    } else {
      x.child.insertSibling(y)
    }
    
    x.degree += 1
  }
  
  private def cut(x: FibonacciHeapNode[T], y: FibonacciHeapNode[T]): Unit = {
    x.remove()
    x.parent = null
    x.mark = false
    min = mergeLists(min, x)
  }
  
  private def cascadingCut(y: FibonacciHeapNode[T]): Unit = {
    val z = y.parent
    if (z != null) {
      if (!y.mark) {
        y.mark = true
      } else {
        cut(y, z)
        cascadingCut(z)
      }
    }
  }
  
  def isEmpty: Boolean = min == null
  def size: Int = numNodes
}

// Example usage
object FibonacciHeapExample extends App {
  val fibHeap = new FibonacciHeap[Int]()
  
  // Insert elements
  println("Inserting elements: 10, 5, 15, 3, 8")
  fibHeap.insert(10)
  fibHeap.insert(5)
  fibHeap.insert(15)
  fibHeap.insert(3)
  fibHeap.insert(8)
  
  println(s"Size: ${fibHeap.size}")
  println(s"Minimum: ${fibHeap.minimum}")
  
  // Extract minimum
  println("\nExtracting minimum elements:")
  while (!fibHeap.isEmpty) {
    val min = fibHeap.extractMin()
    println(s"Extracted: $min")
  }
  
  // Test decrease key
  println("\nTesting decrease key:")
  val fibHeap2 = new FibonacciHeap[Int]()
  val node1 = fibHeap2.insert(10)
  val node2 = fibHeap2.insert(5)
  val node3 = fibHeap2.insert(15)
  
  println(s"Before decrease key: ${fibHeap2.minimum}")
  fibHeap2.decreaseKey(node3, 1)
  println(s"After decreasing node3 to 1: ${fibHeap2.minimum}")
}
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains key, value, parent, child, left/right pointers, degree, and mark flag
2. **Core Operations**:
   - `insert()`: O(1) amortized
   - `extractMin()`: O(log n) amortized
   - `decreaseKey()`: O(1) amortized
   - `delete()`: O(log n) amortized

3. **Advanced Features**:
   - Consolidation with array-based degree tracking
   - Cascading cut for maintaining heap property
   - Lazy deletion of marked nodes

4. **Time Complexity**:
   - Insert: O(1)
   - Extract Min: O(log n)
   - Decrease Key: O(1)
   - Delete: O(log n)

This implementation demonstrates the Fibonacci heap's efficiency in scenarios requiring frequent decrease-key operations, such as Dijkstra's algorithm or Prim's algorithm for minimum spanning trees.


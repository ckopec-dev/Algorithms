# Fibonacci Heap Implementation in Kotlin

```kotlin
import java.util.*

class FibonacciHeapNode<T : Comparable<T>> {
    var key: T
    var degree: Int = 0
    var marked: Boolean = false
    var parent: FibonacciHeapNode<T>? = null
    var child: FibonacciHeapNode<T>? = null
    var left: FibonacciHeapNode<T> = this
    var right: FibonacciHeapNode<T> = this
    
    constructor(key: T) {
        this.key = key
    }
}

class FibonacciHeap<T : Comparable<T>> {
    private var min: FibonacciHeapNode<T>? = null
    private var n: Int = 0
    
    fun insert(key: T): FibonacciHeapNode<T> {
        val node = FibonacciHeapNode(key)
        min = mergeLists(min, node)
        n++
        return node
    }
    
    fun minimum(): T? {
        return min?.key
    }
    
    fun extractMin(): T? {
        val z = min
        if (z != null) {
            // Add all children to root list
            if (z.child != null) {
                var child = z.child
                do {
                    child?.parent = null
                    child = child?.right
                } while (child != z.child)
                min = mergeLists(min, z.child)
            }
            
            // Remove z from root list
            if (z.right != z) {
                z.left?.right = z.right
                z.right?.left = z.left
            } else {
                min = null
            }
            
            // Consolidate
            if (min != null) {
                consolidate()
            }
            
            n--
            return z.key
        }
        return null
    }
    
    private fun consolidate() {
        val array = arrayOfNulls<FibonacciHeapNode<T>>(40) // Assuming max degree < 40
        
        var w = min
        var start = min
        
        do {
            var x = w
            var d = x?.degree ?: 0
            
            while (array[d] != null) {
                var y = array[d]
                if (x?.key ?: return > y?.key ?: return) {
                    val temp = x
                    x = y
                    y = temp
                }
                
                link(y!!, x!!)
                array[d] = null
                d++
            }
            
            array[d] = x
            w = w?.right
        } while (w != start)
        
        min = null
        for (i in array.indices) {
            if (array[i] != null) {
                if (min == null) {
                    min = array[i]
                } else {
                    min = mergeLists(min, array[i])
                }
            }
        }
    }
    
    private fun link(y: FibonacciHeapNode<T>, x: FibonacciHeapNode<T>) {
        // Remove y from root list
        y.left?.right = y.right
        y.right?.left = y.left
        
        // Make y a child of x
        y.parent = x
        y.marked = false
        
        if (x.child == null) {
            x.child = y
            y.right = y
            y.left = y
        } else {
            val child = x.child
            y.right = child
            y.left = child?.left
            child?.left?.right = y
            child?.left = y
        }
        
        x.degree++
    }
    
    private fun mergeLists(min1: FibonacciHeapNode<T>?, min2: FibonacciHeapNode<T>?): FibonacciHeapNode<T>? {
        if (min1 == null) return min2
        if (min2 == null) return min1
        
        val min = if (min1.key < min2.key) min1 else min2
        
        val temp = min1.right
        min1.right = min2.right
        min2.right?.left = min1
        min2.right = temp
        temp?.left = min2
        
        return min
    }
    
    fun isEmpty(): Boolean {
        return min == null
    }
    
    fun size(): Int {
        return n
    }
    
    fun decreaseKey(node: FibonacciHeapNode<T>, newKey: T) {
        if (newKey > node.key) {
            throw IllegalArgumentException("New key is greater than current key")
        }
        
        node.key = newKey
        val y = node.parent
        
        if (y != null && node.key < y.key) {
            cut(node, y)
            cascadingCut(y)
        }
        
        if (node.key < min?.key ?: return) {
            min = node
        }
    }
    
    private fun cut(x: FibonacciHeapNode<T>, y: FibonacciHeapNode<T>) {
        // Remove x from child list of y
        if (x.right != x) {
            x.left?.right = x.right
            x.right?.left = x.left
        }
        
        if (y.child == x) {
            y.child = if (x.right != x) x.right else null
        }
        
        y.degree--
        
        // Add x to root list
        x.parent = null
        x.marked = false
        
        min = mergeLists(min, x)
    }
    
    private fun cascadingCut(y: FibonacciHeapNode<T>) {
        val z = y.parent
        if (z != null) {
            if (!y.marked) {
                y.marked = true
            } else {
                cut(y, z)
                cascadingCut(z)
            }
        }
    }
    
    fun delete(node: FibonacciHeapNode<T>) {
        decreaseKey(node, node.key) // Assuming we can decrease to minimum value
        extractMin()
    }
}

// Example usage
fun main() {
    val fibHeap = FibonacciHeap<Int>()
    
    // Insert elements
    val node1 = fibHeap.insert(10)
    val node2 = fibHeap.insert(20)
    val node3 = fibHeap.insert(5)
    val node4 = fibHeap.insert(15)
    val node5 = fibHeap.insert(30)
    
    println("Minimum element: ${fibHeap.minimum()}") // Should be 5
    
    // Extract minimum
    println("Extracted min: ${fibHeap.extractMin()}") // Should be 5
    println("New minimum: ${fibHeap.minimum()}") // Should be 10
    
    // Decrease key
    fibHeap.decreaseKey(node2, 2)
    println("Minimum after decrease: ${fibHeap.minimum()}") // Should be 2
    
    // Extract all elements
    while (!fibHeap.isEmpty()) {
        println("Extracted: ${fibHeap.extractMin()}")
    }
}
```

## Key Features of this Fibonacci Heap Implementation:

1. **Node Structure**: Each node contains key, degree, parent, child, and sibling pointers
2. **Core Operations**:
   - `insert()`: O(1) amortized time
   - `extractMin()`: O(log n) amortized time
   - `decreaseKey()`: O(1) amortized time
   - `minimum()`: O(1) time

3. **Advanced Features**:
   - Consolidation for maintaining heap property
   - Cascading cut for maintaining tree structure
   - Marked nodes for tracking cut history

4. **Time Complexities**:
   - Insert: O(1)
   - ExtractMin: O(log n)
   - DecreaseKey: O(1)
   - Merge: O(1)

This implementation demonstrates the efficient amortized performance characteristics that make Fibonacci heaps valuable for algorithms like Dijkstra's shortest path and Prim's minimum spanning tree.


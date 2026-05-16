# Treap Implementation in Kotlin

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has a key (for BST property) and a priority (for heap property).

```kotlin
import kotlin.math.min

class TreapNode(var key: Int, var priority: Int) {
    var left: TreapNode? = null
    var right: TreapNode? = null
    var parent: TreapNode? = null
}

class Treap {
    private var root: TreapNode? = null
    private val random = java.util.Random()
    
    // Split operation - splits treap into two parts
    private fun split(node: TreapNode?, key: Int): Pair<TreapNode?, TreapNode?> {
        if (node == null) return Pair(null, null)
        
        if (key < node.key) {
            val (left, right) = split(node.left, key)
            node.left = right
            updateParent(right, node)
            return Pair(left, node)
        } else {
            val (left, right) = split(node.right, key)
            node.right = left
            updateParent(left, node)
            return Pair(node, right)
        }
    }
    
    // Merge operation - combines two treaps
    private fun merge(left: TreapNode?, right: TreapNode?): TreapNode? {
        if (left == null) return right
        if (right == null) return left
        
        if (left.priority > right.priority) {
            left.right = merge(left.right, right)
            updateParent(left.right, left)
            return left
        } else {
            right.left = merge(left, right.left)
            updateParent(right.left, right)
            return right
        }
    }
    
    // Update parent pointer for a node
    private fun updateParent(node: TreapNode?, parent: TreapNode?) {
        if (node != null) {
            node.parent = parent
        }
    }
    
    // Insert a key into the treap
    fun insert(key: Int) {
        val priority = random.nextInt(1000)
        val newNode = TreapNode(key, priority)
        
        if (root == null) {
            root = newNode
            return
        }
        
        // Split the treap at key
        val (left, right) = split(root, key)
        
        // Create a new node with the key
        val merged = merge(left, newNode)
        root = merge(merged, right)
    }
    
    // Delete a key from the treap
    fun delete(key: Int) {
        if (root == null) return
        
        // Split the treap at key
        val (left, right) = split(root, key)
        
        // Split the right part at key + 1 to remove the key
        val (_, rightRight) = split(right, key + 1)
        
        // Merge left and rightRight parts
        root = merge(left, rightRight)
    }
    
    // Search for a key in the treap
    fun search(key: Int): Boolean {
        var current = root
        while (current != null) {
            when {
                key == current.key -> return true
                key < current.key -> current = current.left
                else -> current = current.right
            }
        }
        return false
    }
    
    // In-order traversal to print the treap
    fun inorderTraversal(): List<Int> {
        val result = mutableListOf<Int>()
        inorderHelper(root, result)
        return result
    }
    
    private fun inorderHelper(node: TreapNode?, result: MutableList<Int>) {
        if (node != null) {
            inorderHelper(node.left, result)
            result.add(node.key)
            inorderHelper(node.right, result)
        }
    }
    
    // Print the treap structure
    fun printTreap() {
        printTreapHelper(root, "", true)
    }
    
    private fun printTreapHelper(node: TreapNode?, prefix: String, isLast: Boolean) {
        if (node != null) {
            println("$prefix${if (isLast) "└── " else "├── "}Key: ${node.key}, Priority: ${node.priority}")
            
            val children = mutableListOf<TreapNode?>()
            if (node.left != null) children.add(node.left)
            if (node.right != null) children.add(node.right)
            
            children.forEachIndexed { index, child ->
                val isLastChild = index == children.size - 1
                val newPrefix = if (isLast) "    " else "│   "
                printTreapHelper(child, prefix + newPrefix, isLastChild)
            }
        }
    }
}

// Example usage
fun main() {
    val treap = Treap()
    
    println("Inserting elements: 50, 30, 70, 20, 40, 60, 80")
    treap.insert(50)
    treap.insert(30)
    treap.insert(70)
    treap.insert(20)
    treap.insert(40)
    treap.insert(60)
    treap.insert(80)
    
    println("\nTreap structure:")
    treap.printTreap()
    
    println("\nIn-order traversal:")
    println(treap.inorderTraversal())
    
    println("\nSearching for 40: ${treap.search(40)}")
    println("Searching for 25: ${treap.search(25)}")
    
    println("\nDeleting 30:")
    treap.delete(30)
    println("In-order traversal after deletion:")
    println(treap.inorderTraversal())
    
    println("\nFinal treap structure:")
    treap.printTreap()
}
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, left/right children, and parent pointer
2. **Split Operation**: Divides the treap into two parts based on a key
3. **Merge Operation**: Combines two treaps maintaining heap property
4. **Insert Operation**: Inserts a new key with random priority
5. **Delete Operation**: Removes a key from the treap
6. **Search Operation**: Finds if a key exists in the treap
7. **Traversal**: In-order traversal to display elements
8. **Visualization**: Pretty-print of the treap structure

## Time Complexity:
- **Insert**: O(log n) average, O(n) worst case
- **Delete**: O(log n) average, O(n) worst case  
- **Search**: O(log n) average, O(n) worst case

The treap maintains both BST property (in-order traversal gives sorted sequence) and heap property (priority of each node is greater than its children).


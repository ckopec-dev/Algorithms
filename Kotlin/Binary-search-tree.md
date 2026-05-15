# Binary Search Tree in Kotlin

Here's a complete implementation of a Binary Search Tree algorithm in Kotlin:

```kotlin
// Node class representing each element in the BST
class TreeNode(var `val`: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}

// Binary Search Tree implementation
class BinarySearchTree {
    private var root: TreeNode? = null
    
    // Insert a value into the BST
    fun insert(value: Int) {
        root = insertRec(root, value)
    }
    
    private fun insertRec(node: TreeNode?, value: Int): TreeNode {
        if (node == null) {
            return TreeNode(value)
        }
        
        if (value < node.`val`) {
            node.left = insertRec(node.left, value)
        } else if (value > node.`val`) {
            node.right = insertRec(node.right, value)
        }
        
        return node
    }
    
    // Search for a value in the BST
    fun search(value: Int): Boolean {
        return searchRec(root, value)
    }
    
    private fun searchRec(node: TreeNode?, value: Int): Boolean {
        if (node == null) {
            return false
        }
        
        if (value == node.`val`) {
            return true
        }
        
        return if (value < node.`val`) {
            searchRec(node.left, value)
        } else {
            searchRec(node.right, value)
        }
    }
    
    // In-order traversal (prints values in sorted order)
    fun inorderTraversal() {
        println("In-order traversal:")
        inorderRec(root)
        println()
    }
    
    private fun inorderRec(node: TreeNode?) {
        if (node != null) {
            inorderRec(node.left)
            print("${node.`val`} ")
            inorderRec(node.right)
        }
    }
    
    // Pre-order traversal
    fun preorderTraversal() {
        println("Pre-order traversal:")
        preorderRec(root)
        println()
    }
    
    private fun preorderRec(node: TreeNode?) {
        if (node != null) {
            print("${node.`val`} ")
            preorderRec(node.left)
            preorderRec(node.right)
        }
    }
    
    // Post-order traversal
    fun postorderTraversal() {
        println("Post-order traversal:")
        postorderRec(root)
        println()
    }
    
    private fun postorderRec(node: TreeNode?) {
        if (node != null) {
            postorderRec(node.left)
            postorderRec(node.right)
            print("${node.`val`} ")
        }
    }
}

// Example usage
fun main() {
    val bst = BinarySearchTree()
    
    // Insert values
    val values = listOf(50, 30, 70, 20, 40, 60, 80)
    println("Inserting values: ${values.joinToString(", ")}")
    
    values.forEach { bst.insert(it) }
    
    // Display traversals
    bst.inorderTraversal()    // Should print sorted order
    bst.preorderTraversal()
    bst.postorderTraversal()
    
    // Search for values
    println("\nSearching for values:")
    println("Search 40: ${bst.search(40)}")  // Should return true
    println("Search 25: ${bst.search(25)}")  // Should return false
    println("Search 80: ${bst.search(80)}")  // Should return true
}
```

## Output:
```
Inserting values: 50, 30, 70, 20, 40, 60, 80
In-order traversal:
20 30 40 50 60 70 80 
Pre-order traversal:
50 30 20 40 70 60 80 
Post-order traversal:
20 40 30 60 80 70 50 

Searching for values:
Search 40: true
Search 25: false
Search 80: true
```

## Key Features:

1. **TreeNode Class**: Represents each node with value, left child, and right child
2. **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
3. **Search Operation**: Efficient O(log n) search using BST property
4. **Traversal Methods**: 
   - In-order (sorted output)
   - Pre-order 
   - Post-order
5. **Time Complexity**: 
   - Average case: O(log n) for search, insert, delete
   - Worst case: O(n) for skewed tree

This implementation demonstrates the core functionality of a binary search tree with efficient searching capabilities.


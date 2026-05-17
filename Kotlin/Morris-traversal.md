# Morris Traversal Algorithm in Kotlin

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's right pointers to create temporary links, allowing for in-order traversal with O(1) space complexity.

```kotlin
// Definition for a binary tree node
class TreeNode(var `val`: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}

class MorrisTraversal {
    
    /**
     * Morris In-order Traversal
     * Time Complexity: O(n)
     * Space Complexity: O(1)
     */
    fun morrisInOrder(root: TreeNode?): List<Int> {
        val result = mutableListOf<Int>()
        var current = root
        
        while (current != null) {
            if (current.left == null) {
                // Process current node
                result.add(current.`val`)
                current = current.right
            } else {
                // Find the inorder predecessor
                var predecessor = current.left
                while (predecessor?.right != null && predecessor.right != current) {
                    predecessor = predecessor.right
                }
                
                if (predecessor?.right == null) {
                    // Make current as right child of predecessor
                    predecessor?.right = current
                    current = current.left
                } else {
                    // Revert the changes (restore tree structure)
                    predecessor.right = null
                    // Process current node
                    result.add(current.`val`)
                    current = current.right
                }
            }
        }
        
        return result
    }
    
    /**
     * Morris Pre-order Traversal
     */
    fun morrisPreOrder(root: TreeNode?): List<Int> {
        val result = mutableListOf<Int>()
        var current = root
        
        while (current != null) {
            if (current.left == null) {
                result.add(current.`val`)
                current = current.right
            } else {
                var predecessor = current.left
                while (predecessor?.right != null && predecessor.right != current) {
                    predecessor = predecessor.right
                }
                
                if (predecessor?.right == null) {
                    result.add(current.`val`) // Add before traversing left
                    predecessor?.right = current
                    current = current.left
                } else {
                    predecessor.right = null
                    current = current.right
                }
            }
        }
        
        return result
    }
}

// Example usage
fun main() {
    // Create a sample binary tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)
    root.left?.left = TreeNode(4)
    root.left?.right = TreeNode(5)
    
    val morris = MorrisTraversal()
    
    // In-order traversal: 4 2 5 1 3
    println("In-order traversal: ${morris.morrisInOrder(root)}")
    
    // Pre-order traversal: 1 2 4 5 3
    println("Pre-order traversal: ${morris.morrisPreOrder(root)}")
}
```

## How Morris Traversal Works:

1. **For In-order traversal:**
   - If current node has no left child, process it and move to right
   - If current node has left child:
     - Find the inorder predecessor (rightmost node in left subtree)
     - If predecessor's right is null, make current as its right child and move to left
     - If predecessor's right is current, revert the changes and process current node

2. **For Pre-order traversal:**
   - Process current node before moving to left subtree
   - Follow the same linking/reverting pattern as in-order

## Key Advantages:
- **Space Efficient**: O(1) space complexity
- **No recursion or stack**: Uses existing tree pointers
- **Same time complexity**: O(n) for all traversals

## Output:
```
In-order traversal: [4, 2, 5, 1, 3]
Pre-order traversal: [1, 2, 4, 5, 3]
```

